(require 'grizzl)


;; Global variables
;; =================================================================================

;; Customizable

(defcustom unimacs/read-max-results 10
  "The maximum number of results to show.")

(defcustom unimacs/views
  '((buffers . ((unimacs/src-buffers)))
    (extended . ((unimacs/src-extended))))
  "Available views.")


;; Current only to a search

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


;; Minor mode used when searching
;; =================================================================================

(defcustom unimacs/keymap (make-sparse-keymap)
  "Internal keymap used by the minor-mode in `unimacs/completing-read'.")

(define-key unimacs/keymap (kbd "<up>")   'unimacs/set-selection+1)
(define-key unimacs/keymap (kbd "M-k")    'unimacs/set-selection+1)
(define-key unimacs/keymap (kbd "<down>") 'unimacs/set-selection-1)
(define-key unimacs/keymap (kbd "M-j")    'unimacs/set-selection-1)


(define-minor-mode unimacs/mode
  "Toggle the internal mode used by `unimacs/completing-read'."
  nil
  " Unimacs"
  unimacs/keymap)



;; Search functionality
;; =================================================================================
;; These functions are heavily inspired by the grizzl-read module by Chris Corbyn.
;; They have been modified to display auxiliary data.


(defun unimacs/selected-result (index)
  (elt (grizzl-result-strings *unimacs/result* index
                              :start 0
                              :end   unimacs/read-max-results)
       (unimacs/current-selection)))


(defun unimacs/format-prompt-line (prompt)
  (let* ((count (grizzl-result-count *unimacs/result*))
         (match-info (format " (%d candidate%s) ---- *-"
                             count (if (= count 1) "" "s"))))
    (concat (propertize (format "-*%s *-" prompt) 'face 'modeline-inactive)
            (propertize " "
                        'face    'modeline-inactive
                        'display `(space :align-to (- right
                                                      ,(1+ (length match-info)))))
            (propertize match-info 'face 'modeline-inactive))))


(defun unimacs/current-selection ()
  (let ((max-selection
         (min (1- unimacs/read-max-results)
              (1- (grizzl-result-count *unimacs/result*)))))
    (max 0 (min max-selection *unimacs/selection*))))


(defun unimacs/format-match (match-str selected)
  (let* ((margin (if selected "> " "  "))
         (aux (or (gethash match-str *unimacs/hashmap*) ""))
         (face (if selected 'dired-symlink 'default))
         (str (apply 'concat
                     (cl-mapcar (lambda (s w)
                                  (if s (format (format "%%-%ds   " w) s) ""))
                                (cons match-str aux)
                                *unimacs/widths*))))
    (propertize str 'face face)))


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


(defun unimacs/display-result (index prompt)
  (let* ((matches (grizzl-result-strings *unimacs/result* index
                                         :start 0
                                         :end   unimacs/read-max-results)))
    (delete-all-overlays)
    (overlay-put (make-overlay (point-min) (point-min))
                 'before-string
                 (format "%s\n%s\n"
                         (mapconcat 'identity
                                    (unimacs/map-format-matches matches)
                                    "\n")
                         (unimacs/format-prompt-line prompt)))
    (set-window-text-height nil (max 3 (+ 2 (length matches))))))


(defun unimacs/completing-read (prompt title index)
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
                        (unimacs/display-result index title)))
             (exitfun (lambda ()
                        (unimacs/mode -1)
                        (remove-hook 'post-command-hook    hookfun t))))
          (add-hook 'minibuffer-exit-hook exitfun nil t)
          (add-hook 'post-command-hook    hookfun nil t)))
    (read-from-minibuffer (concat prompt " "))
    (unimacs/selected-result index)))


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


(defun unimacs/prepare-view (view)
  (let* ((view-data (assq view unimacs/views))
         (sources (cdr view-data))
         (changed (reduce 'or (mapcar (lambda (src)
                                        (if (apply (car src) 'changed (cdr src))
                                            (progn (apply (car src) 'update (cdr src))
                                                   t)
                                          nil))
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
          (puthash (car elt) (cdr elt) (cdr (assq 'auxdata (gethash view *unimacs/view-data*)))))))))


(defun unimacs/view (view title callback)
  (unimacs/prepare-view view)
  (let ((vd (gethash view *unimacs/view-data*)))
    (setq *unimacs/index* (cdr (assq 'index vd)))
    (setq *unimacs/ncolumns* (cdr (assq 'ncolumns vd)))
    (setq *unimacs/widths* (cdr (assq 'widths vd)))
    (setq *unimacs/hashmap* (cdr (assq 'auxdata vd))))
  (funcall callback
           (unimacs/completing-read ">>>" (concat " Unimacs: " title) *unimacs/index*)))



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
          (setq *unimacs/src-buffers-data*
                (cons (list bufname
                            (with-current-buffer bufname mode-name)
                            (buffer-file-name (get-buffer bufname)))
                      *unimacs/src-buffers-data*)))))))))



;; Extended commands source
;; =================================================================================

(defvar *unimacs/src-extended-count* -1)
(defvar *unimacs/src-extended-data* nil)

(defun unimacs/src-extended (command)
  (cond
   ((eq 'provide command)
    (dolist (elt *unimacs/src-extended-data*)
      (setq *unimacs/data* (cons elt *unimacs/data*))))

   ((eq 'update command)
    (setq *unimacs/src-extended-data* nil)
    (mapatoms (lambda (smb)
                (if (commandp smb)
                    (setq *unimacs/src-extended-data*
                          (cons (list (symbol-name smb))
                                *unimacs/src-extended-data*))))))

   ((eq 'changed command)
    (let ((i 0))
      (mapatoms (lambda (smb)
                  (when (commandp smb)
                    (setq i (1+ i)))))
      (unless (= i *unimacs/src-extended-count*)
        (setq *unimacs/src-extended-count* i))))))


;; Fin
;; =================================================================================

(provide 'unimacs)
