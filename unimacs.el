(require 'grizzl)
(require 'f)


;; Global variables
;; =================================================================================

;; Customizable

(defcustom unimacs/read-max-results 10
  "The maximum number of results to show.")

(defcustom unimacs/views
  '((buffers . ((unimacs/src-buffers)))
    (extended . ((unimacs/src-extended)))
    (functions . ((unimacs/src-functions)))
    (variables . ((unimacs/src-variables)))
    )
  "Available views.")


;; Current only to a search

(defvar *unimacs/abnormal-exit* nil
  "Truthy if the exit was abnormal, in which case this value is returned.")

(defvar *unimacs/file-prompt* nil
  "Holds the current directory in case of a file view.")

(defvar *unimacs/result* nil
  "The search result.")

(defvar *unimacs/selection* 0
  "The selected offset.")

(defvar *unimacs/hashmap* (make-hash-table)
  "A map mapping search entries to auxiliary data.")

(defvar *unimacs/ncolumns* nil
  "The number of columns in the search (including search entry).")

(defvar *unimacs/widths* nil
  "The widths for each column. A list of length *unimacs/ncolumns*.")


;; Used for sources to communicate information

(defvar *unimacs/data* nil
  "The data to search in. A list of lists of strings.")


;; Persistent over a session

(defvar *unimacs/indexes* (make-hash-table)
  "A grizzl search index for each view.")

(defvar *unimacs/view-data* (make-hash-table)
  "")


;; Faces

(let ((bwc-plain "#f8f6f2") (bwc-snow "#ffffff") (bwc-coal "#000000") (bwc-brightgravel "#d9cec3")
      (bwc-lightgravel "#998f84") (bwc-gravel "#857f78") (bwc-mediumgravel "#666462")
      (bwc-deepgravel "#45413b") (bwc-deepergravel "#35322d") (bwc-darkgravel "#242321")
      (bwc-blackgravel "#1c1b1a") (bwc-blackestgravel "#141413") (bwc-dalespale "#fade3e")
      (bwc-dirtyblonde "#f4cf86") (bwc-taffy "#ff2c4b") (bwc-saltwatertaffy "#8cffba")
      (bwc-tardis "#0a9dff") (bwc-darktardis "#005fff") (bwc-orange "#ffa724") (bwc-lime "#aeee00")
      (bwc-dress "#ff9eb8") (bwc-toffee "#b88853") (bwc-coffee "#c7915b") (bwc-darkroast "#88633f")
      (bwc-term-blue "#6298c8") (bwc-term-green "#82d92f") (bwc-term-yellow "#f3e14c")
      (bwc-term-red "#e5261f"))

  (defface unimacs/normal
    `((t (:foreground ,bwc-snow)))
    "For various things that don't need specific highlighting.")

  (defface unimacs/file
    `((t (:foreground ,bwc-snow)))
    "Files in file views.")

  (defface unimacs/symlink
    `((t (:foreground ,bwc-dress)))
    "Symlinks in file views.")

  (defface unimacs/directory
    `((t (:foreground ,bwc-term-blue :weight bold)))
    "Directories in file views.")

  (defface unimacs/file-other
    `((t (:foreground ,bwc-taffy)))
    "Other kinds of entries in file views.")

  (defface unimacs/buffer-file
    `((t (:foreground ,bwc-snow)))
    "Buffer with a file.")

  (defface unimacs/buffer-other
    `((t (:foreground ,bwc-dress)))
    "Buffer without a file.")
  )


;; Minor mode used when searching
;; =================================================================================

(defcustom unimacs/keymap (make-sparse-keymap)
  "Internal keymap used by the minor-mode in `unimacs/completing-read'.")

(define-key unimacs/keymap (kbd "<up>")   'unimacs/set-selection+1)
(define-key unimacs/keymap (kbd "M-k")    'unimacs/set-selection+1)
(define-key unimacs/keymap (kbd "<down>") 'unimacs/set-selection-1)
(define-key unimacs/keymap (kbd "M-j")    'unimacs/set-selection-1)
(define-key unimacs/keymap (kbd "<tab>")  (kbd "RET"))
(define-key unimacs/keymap (kbd "C-l")    'unimacs/file-up)


(define-minor-mode unimacs/mode
  "Toggle the internal mode used by `unimacs/completing-read'."
  nil
  " Unimacs"
  unimacs/keymap)



;; Search functionality
;; =================================================================================
;; These functions are heavily inspired by the grizzl-read module by Chris Corbyn.
;; They have been modified to remove the modeline and display auxiliary data.


(defun unimacs/selected-result (index)
  (elt (grizzl-result-strings *unimacs/result* index
                              :start 0
                              :end   unimacs/read-max-results)
       (unimacs/current-selection)))


(defun unimacs/current-selection ()
  (let ((max-selection
         (min (1- unimacs/read-max-results)
              (1- (grizzl-result-count *unimacs/result*)))))
    (max 0 (min max-selection *unimacs/selection*))))


(defun unimacs/highlight (str selected)
  (if selected
      (propertize str 'face (list 'hl-line
                                  (get-text-property 0 'face str)))
    str))


(defun unimacs/format-match (match-str selected)
  (let* ((aux (or (gethash match-str *unimacs/hashmap*) ""))
         (str (apply 'concat
                     (cl-mapcar (lambda (s w)
                                  (unimacs/highlight
                                   (if s (format (format "%%-%ds   " w) s) "")
                                   selected))
                                (cons match-str aux)
                                *unimacs/widths*)))
         (righty (unimacs/highlight (propertize " " 'display `(space :align-to right)) selected))
         (fstr (concat str righty)))
    fstr))


(defun unimacs/map-format-matches (matches)
  (if (= 0 (length matches))
      (list (propertize "-- NO MATCH --" 'face 'outline-3))
    (cdr (cl-reduce (lambda (acc str)
                      (let* ((idx (car acc))
                             (lst (cdr acc))
                             (sel (= idx (unimacs/current-selection))))
                        (cons (1+ idx)
                              (cons (unimacs/format-match str sel) lst))))
                    matches
                    :initial-value '(0)))))


(defun unimacs/display-result (index)
  (let* ((matches (grizzl-result-strings *unimacs/result* index
                                         :start 0
                                         :end   unimacs/read-max-results)))
    (delete-all-overlays)
    (overlay-put (make-overlay (point-min) (point-min))
                 'before-string
                 (format "%s\n"
                         (mapconcat 'identity
                                    (unimacs/map-format-matches matches)
                                    "\n")))
    (set-window-text-height nil (max 2 (1+ (length matches))))))


(defun unimacs/completing-read (prompt index)
  (minibuffer-with-setup-hook
      (lambda ()
        (setq *unimacs/result* nil)
        (setq *unimacs/selection* 0)
        (unimacs/mode 1)
        (lexical-let*
            ((hookfun (lambda ()
                        (setq *unimacs/result*
                              (grizzl-search (minibuffer-contents)
                                             index
                                             *unimacs/result*))
                        (unimacs/display-result index)))
             (exitfun (lambda ()
                        (unimacs/mode -1)
                        (remove-hook 'post-command-hook    hookfun t))))
          (add-hook 'minibuffer-exit-hook exitfun nil t)
          (add-hook 'post-command-hook    hookfun nil t)))
    (read-from-minibuffer prompt)
    (if *unimacs/abnormal-exit*
        (let ((retval *unimacs/abnormal-exit*))
          (setq *unimacs/abnormal-exit* nil)
          retval)
      (unimacs/selected-result index))))


(defun unimacs/move-selection (delta)
  (setq *unimacs/selection* (+ (unimacs/current-selection) delta))
  (when (not (= (unimacs/current-selection) *unimacs/selection*))
    (beep)))


(defun unimacs/set-selection+1 ()
  (interactive)
  (unimacs/move-selection 1))


(defun unimacs/set-selection-1 ()
  (interactive)
  (unimacs/move-selection -1))


(defun unimacs/max-lengths (lengths item)
  (if item
      (cons (max (car lengths) (length (car item)))
            (unimacs/max-lengths (cdr lengths) (cdr item)))
    lengths))


(defun unimacs/prepare-sources (view sources)
  (let ((changed (reduce 'or (mapcar (lambda (src)
                                       (when (apply (car src) 'changed (cdr src))
                                         (apply (car src) 'update (cdr src))
                                         t))
                                     sources))))
    (when (or changed (not (gethash view *unimacs/view-data*)))
      (setq *unimacs/data* nil)
      (dolist (elt sources)
        (apply (car elt) 'provide (cdr elt)))
      (let ((nc (apply 'max (mapcar 'length *unimacs/data*))))
        (puthash view `((index . ,(grizzl-make-index (mapcar 'car *unimacs/data*)))
                        (auxdata . ,(make-hash-table))
                        (ncolumns . ,nc)
                        (widths . ,(cl-reduce 'unimacs/max-lengths
                                              *unimacs/data*
                                              :initial-value (make-list nc 0))))
                 *unimacs/view-data*))
      (let ((vd (gethash view *unimacs/view-data*)))
        (dolist (elt *unimacs/data*)
          (puthash (car elt) (cdr elt) (cdr (assq 'auxdata vd))))))))


(defun unimacs/prepare-view (view)
  (let* ((view-data (assq view unimacs/views))
         (sources (cdr view-data)))
    (unimacs/prepare-sources view sources)))


(defun unimacs/set-view (view)
  (let ((vd (gethash view *unimacs/view-data*)))
    (setq *unimacs/index* (cdr (assq 'index vd)))
    (setq *unimacs/ncolumns* (cdr (assq 'ncolumns vd)))
    (setq *unimacs/widths* (cdr (assq 'widths vd)))
    (setq *unimacs/hashmap* (cdr (assq 'auxdata vd)))))


(defun unimacs/view (view callback)
  (unimacs/prepare-view view)
  (unimacs/set-view view)
  (funcall callback (unimacs/completing-read ">>> " *unimacs/index*)))


(defun unimacs/view-files (directory callback)
  (let ((sources `((unimacs/src-files ,directory)))
        (prompt (f-slash (f-short directory))))
    (unimacs/prepare-sources 'files sources)
    (unimacs/set-view 'files)
    (setq *unimacs/file-prompt* directory)
    (funcall callback (unimacs/completing-read prompt *unimacs/index*))
    (setq *unimacs/file-prompt* nil)))


(defun unimacs/file-up ()
  (interactive)
  (when *unimacs/file-prompt*
    (setq *unimacs/abnormal-exit* (f-parent *unimacs/file-prompt*))
    (exit-minibuffer)))



;; Buffer source
;; =================================================================================

(defvar *unimacs/src-buffers-checksum* nil)
(defvar *unimacs/src-buffers-data* nil)

(defun unimacs/src-buffers (command)
  (cond
   ((eq 'provide command)
    (dolist (elt *unimacs/src-buffers-data*)
      (setq *unimacs/data* (cons elt *unimacs/data*))))

   ((or (eq 'changed command) (eq 'update command))
    (let* ((pre-buffers (mapcar 'buffer-name (buffer-list)))
           (filt-buffers (delq nil (mapcar (lambda (s)
                                             (if (eq 32 (string-to-char s)) nil s))
                                           pre-buffers))))
      (cond
       ((eq 'changed command)
        (let ((checksum (secure-hash 'md5 (apply 'concat filt-buffers))))
          (unless (string= checksum *unimacs/src-buffers-checksum*)
            (setq *unimacs/src-buffers-checksum* checksum))))

       ((eq 'update command)
        (setq *unimacs/src-buffers-data* nil)
        (dolist (bufname filt-buffers)
          (let ((face (if (buffer-file-name (get-buffer bufname))
                          'unimacs/buffer-file 'unimacs/buffer-other))
                (mode (with-current-buffer bufname mode-name))
                (fn (or (buffer-file-name (get-buffer bufname)) "")))
            (setq *unimacs/src-buffers-data*
                  (cons (list (propertize bufname 'face face)
                              (propertize mode 'face face)
                              (propertize fn 'face face))
                        *unimacs/src-buffers-data*))))))))))



;; Meta source for querying obarray
;; =================================================================================

(defvar *unimacs/src-obarray-counts* (make-hash-table))
(defvar *unimacs/src-obarray-datas* (make-hash-table))

(defun unimacs/src-obarray (type predicate command)
  (unless (gethash type *unimacs/src-obarray-counts*)
    (puthash type -1 *unimacs/src-obarray-counts*)
    (puthash type nil *unimacs/src-obarray-datas*))

  (cond
   ((eq 'provide command)
    (dolist (elt (gethash type *unimacs/src-obarray-datas*))
      (setq *unimacs/data* (cons elt *unimacs/data*))))

   ((eq 'update command)
    (let (lst)
      (mapatoms (lambda (smb)
                  (when (funcall predicate smb)
                    (setq lst
                          (cons (list (propertize (symbol-name smb) 'face 'unimacs/normal))
                                lst)))))
      (puthash type lst *unimacs/src-obarray-datas*)))

   ((eq 'changed command)
    (let ((i 0))
      (mapatoms (lambda (smb)
                  (when (funcall predicate smb)
                    (setq i (1+ i)))))
      (unless (= i (gethash type *unimacs/src-obarray-counts*))
        (puthash type i *unimacs/src-obarray-counts*))))))


(defun unimacs/src-extended (command)
  (unimacs/src-obarray 'extended 'commandp command))

(defun unimacs/src-functions (command)
  (unimacs/src-obarray 'functions 'fboundp command))

(defun unimacs/src-variables (command)
  (unimacs/src-obarray 'variables
                       (lambda (vv)
                         (or (get vv 'variable-documentation)
                             (and (boundp vv) (not (keywordp vv)))))
                       command))



;; Filenames source
;; =================================================================================

(defun unimacs/src-files (command directory)
  (cond
   ((eq 'changed command) t)
   ((eq 'update command))
   ((eq 'provide command)
    (f-entries
     directory
     (lambda (fn)
       (let ((face (cond ((f-symlink? fn) 'unimacs/symlink)
                         ((f-file? fn) 'unimacs/file)
                         ((f-directory? fn) 'unimacs/directory)
                         (t 'unimacs/file-other)))
             (stat (file-attributes (f-canonical fn))))
         (setq *unimacs/data*
               (cons (list
                      (propertize (f-relative fn directory) 'face face)
                      (propertize (number-to-string (if stat (nth 7 stat) "")) 'face face)
                      (propertize (if stat (current-time-string (nth 5 stat)) "") 'face face))
                     *unimacs/data*))))))))



;; Standard commands
;; =================================================================================

(defun unimacs/cmd-switch-buffer ()
  (interactive)
  (unimacs/view 'buffers 'switch-to-buffer))

(defun unimacs/cmd-extended-command ()
  (interactive)
  (unimacs/view 'extended
                (lambda (cmd)
                  (execute-extended-command current-prefix-arg cmd))))

(defun unimacs/cmd-describe-function ()
  (interactive)
  (unimacs/view 'functions (lambda (f)
                             (describe-function (intern f)))))

(defun unimacs/cmd-describe-variable ()
  (interactive)
  (unimacs/view 'variables (lambda (f)
                             (describe-variable (intern f)))))

(defun unimacs/cmd-find-file (&optional dir-in)
  (interactive)
  (let ((dir (or dir-in default-directory)))
    (if (f-directory? dir)
        (unimacs/view-files dir (lambda (f)
                                  (unimacs/cmd-find-file (f-expand f dir))))
      (find-file dir))))



;; Fin
;; =================================================================================

(provide 'unimacs)
