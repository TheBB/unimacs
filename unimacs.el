(require 'grizzl)


(defvar *unimacs/read-max-results* 10
  "The maximum number of results to show.")

(defvar *unimacs/result* nil
  "The search result.")

(defvar *unimacs/hashmap* (make-hash-table)
  "The hashmap containing auxiliary data.")

(defvar *unimacs/index* nil
  "The search index.")

(defvar *unimacs/ncolumns* nil
  "The number of columns.")

(defvar *unimacs/widths* nil
  "The widths for each column.")

(defvar *unimacs/data* nil
  "The data to search in.")

(defvar *unimacs/selection* 0
  "The selected offset.")


(defun unimacs/selected-result (index)
  (elt (grizzl-result-strings *unimacs/result* index
                              :start 0
                              :end   *unimacs/read-max-results*)
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
         (min (1- *unimacs/read-max-results*)
              (1- (grizzl-result-count *unimacs/result*)))))
    (max 0 (min max-selection *unimacs/selection*))))


(defun unimacs/format-match (match-str selected)
  (let* ((margin (if selected "> " "  "))
         (aux (or (gethash match-str *unimacs/hashmap*) ""))
         (face (if selected 'diredp-symlink 'default))
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
                                         :end   *unimacs/read-max-results*)))
    (delete-all-overlays)
    (overlay-put (make-overlay (point-min) (point-min))
                 'before-string
                 (format "%s\n%s\n"
                         (mapconcat 'identity
                                    (unimacs/map-format-matches matches)
                                    "\n")
                         (unimacs/format-prompt-line prompt)))
    (set-window-text-height nil (max 3 (+ 2 (length matches))))))


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
                        (unimacs/display-result index prompt)))
             (exitfun (lambda ()
                        (unimacs/mode -1)
                        (remove-hook 'post-command-hook    hookfun t))))
          (add-hook 'minibuffer-exit-hook exitfun nil t)
          (add-hook 'post-command-hook    hookfun nil t)))
    (read-from-minibuffer ">>> ")
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


(defvar *unimacs/keymap* (make-sparse-keymap)
  "Internal keymap used by the minor-mode in `unimacs/completing-read'.")

(define-key *unimacs/keymap* (kbd "<up>")   'unimacs/set-selection+1)
(define-key *unimacs/keymap* (kbd "M-k")    'unimacs/set-selection+1)
(define-key *unimacs/keymap* (kbd "<down>") 'unimacs/set-selection-1)
(define-key *unimacs/keymap* (kbd "M-j")    'unimacs/set-selection-1)


(define-minor-mode unimacs/mode
  "Toggle the internal mode used by `unimacs/completing-read'."
  nil
  " Unimacs"
  *unimacs/keymap*)


(defun unimacs/max-lengths (lengths item)
  (if item
      (cons (max (car lengths) (length (car item)))
            (unimacs/max-lengths (cdr lengths) (cdr item)))
    lengths))


(defun unimacs/do ()
  (setq *unimacs/index* (grizzl-make-index (mapcar 'car *unimacs/data*)))
  (setq *unimacs/ncolumns* (apply 'max (mapcar 'length *unimacs/data*)))
  (setq *unimacs/widths*
        (cl-reduce 'unimacs/max-lengths
                   *unimacs/data*
                   :initial-value (make-list *unimacs/ncolumns* 0)))
  (clrhash *unimacs/hashmap*)
  (dolist (elt *unimacs/data*)
    (puthash (car elt) (cdr elt) *unimacs/hashmap*))
  (unimacs/completing-read " Test" *unimacs/index*))


(defun unimacs/buffers ()
  (interactive)
  (let* ((pre-buffers (mapcar 'buffer-name (buffer-list)))
         (filt-buffers (delq nil (mapcar (lambda (s)
                                           (if (eq 32 (string-to-char s)) nil s))
                                         pre-buffers))))
    (setq *unimacs/data* (mapcar (lambda (bufname)
                                   (list bufname
                                         (with-current-buffer bufname mode-name)
                                         (buffer-file-name (get-buffer bufname))))
                                 filt-buffers))
    (unimacs/do)
    ))


(provide 'unimacs)
