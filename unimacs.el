(require 'grizzl)


;; Global variables
;; =================================================================================

(defcustom unimacs/read-max-results 10
  "The maximum number of results to show.")

(defvar *unimacs/result* nil
  "The search result.")

(defvar *unimacs/hashmap* (make-hash-table)
  "A map mapping search entries to auxiliary data.")

(defvar *unimacs/index* nil
  "The grizzl search index.")

(defvar *unimacs/ncolumns* nil
  "The number of columns in the search (including search entry).")

(defvar *unimacs/widths* nil
  "The widths for each column. A list of length *unimacs/ncolumns*.")

(defvar *unimacs/data* nil
  "The data to search in. A list of lists of strings.")

(defvar *unimacs/selection* 0
  "The selected offset.")

(defcustom unimacs/views
  '((buffers . ((unimacs/src-buffers))))
  "Available views.")



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


(defun unimacs/execute (title)
  (setq *unimacs/index* (grizzl-make-index (mapcar 'car *unimacs/data*)))
  (setq *unimacs/ncolumns* (apply 'max (mapcar 'length *unimacs/data*)))
  (setq *unimacs/widths*
        (cl-reduce 'unimacs/max-lengths
                   *unimacs/data*
                   :initial-value (make-list *unimacs/ncolumns* 0)))
  (clrhash *unimacs/hashmap*)
  (dolist (elt *unimacs/data*)
    (puthash (car elt) (cdr elt) *unimacs/hashmap*))
  (unimacs/completing-read ">>>" (concat " Unimacs: " title) *unimacs/index*))


(defun unimacs/reset ()
  (setq *unimacs/data* nil))


(defun unimacs/view (view title callback)
  (unimacs/reset)
  (let* ((view-data (assq view unimacs/views))
         (sources (cdr view-data)))
    (dolist (elt sources)
      (apply (car elt) (cdr elt)))
    (funcall callback (unimacs/execute title))))



;; Sources
;; =================================================================================


(defun unimacs/src-buffers ()
  (let* ((pre-buffers (mapcar 'buffer-name (buffer-list)))
         (filt-buffers (delq nil (mapcar (lambda (s)
                                           (if (eq 32 (string-to-char s)) nil s))
                                         pre-buffers))))
    (dolist (bufname filt-buffers)
      (setq *unimacs/data*
            (cons (list bufname
                        (with-current-buffer bufname mode-name)
                        (buffer-file-name (get-buffer bufname)))
                  *unimacs/data*)))))



;; Fin
;; =================================================================================

(provide 'unimacs)
