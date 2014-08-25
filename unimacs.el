(require 'grizzl)


(defvar *unimacs/read-max-results* 10
  "The maximum number of results to show.")

(defvar *unimacs/current-result* nil
  "The search result.")

(defvar *unimacs/current-hashmap* (make-hash-table)
  "The hashmap containing auxiliary data.")

(defvar *unimacs/max-length* 60
  "Maximal length among strings being searched.")

(defvar *unimacs/current-selection* 0
  "The selected offset.")


(defun unimacs/selected-result (index)
  (elt (grizzl-result-strings *unimacs/current-result* index
                              :start 0
                              :end   *unimacs/read-max-results*)
       (unimacs/current-selection)))


(defun unimacs/format-prompt-line (prompt)
  (let* ((count (grizzl-result-count *unimacs/current-result*))
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
              (1- (grizzl-result-count *unimacs/current-result*)))))
    (max 0 (min max-selection *unimacs/current-selection*))))


(defun unimacs/format-match (match-str selected)
  (let ((margin (if selected "> " "  "))
        (aux    (or (gethash match-str *unimacs/current-hashmap*) ""))
        (face   (if selected 'diredp-symlink 'default)))
    (propertize (format (format "%%s%%-%ds   %%s" *unimacs/max-length*)
                        margin match-str aux)
                'face face)))


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
  (let* ((matches (grizzl-result-strings *unimacs/current-result* index
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
        (setq *unimacs/current-result* nil)
        (setq *unimacs/current-selection* 0)
        (unimacs/mode 1)
        (lexical-let*
            ((hookfun (lambda ()
                        (setq *unimacs/current-result*
                              (grizzl-search (minibuffer-contents)
                                             index
                                             *unimacs/current-result*))
                        (unimacs/display-result index prompt)))
             (exitfun (lambda ()
                        (unimacs/mode -1)
                        (remove-hook 'post-command-hook    hookfun t))))
          (add-hook 'minibuffer-exit-hook exitfun nil t)
          (add-hook 'post-command-hook    hookfun nil t)))
    (read-from-minibuffer ">>> ")
    (unimacs/selected-result index)))


(defun unimacs/move-selection (delta)
  (setq *unimacs/current-selection* (+ (unimacs/current-selection) delta))
  (when (not (= (unimacs/current-selection) *unimacs/current-selection*))
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


(defun unimacs/buffers ()
  (interactive)
  (let* ((pre-buffers (mapcar 'buffer-name (buffer-list)))
         (filt-buffers (delq nil (mapcar (lambda (s)
                                           (if (eq 32 (string-to-char s)) nil s))
                                         pre-buffers)))
         (index (grizzl-make-index filt-buffers)))
    (setq *unimacs/max-length*
          (apply 'max (mapcar 'length filt-buffers)))
    (clrhash *unimacs/current-hashmap*)
    (dolist (elt filt-buffers)
      (puthash elt (buffer-file-name (get-buffer elt)) *unimacs/current-hashmap*))
    (switch-to-buffer (unimacs/completing-read " Unimacs: Switch to buffer" index))))


(provide 'unimacs)
