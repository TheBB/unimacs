(require 'grizzl)


(defun unimacs/completing-read (prompt index)
  "Performs a completing-read in the minibuffer using INDEX to fuzzy search.
Each key pressed in the minibuffer filters down the list of matches."
  (minibuffer-with-setup-hook
      (lambda ()
        (setq *grizzl-current-result* nil)
        (setq *grizzl-current-selection* 0)
        (grizzl-mode 1)
        (lexical-let*
            ((hookfun (lambda ()
                        (setq *grizzl-current-result*
                              (grizzl-search (minibuffer-contents)
                                             index
                                             *grizzl-current-result*))
                        (grizzl-display-result index prompt)))
             (exitfun (lambda ()
                        (grizzl-mode -1)
                        (remove-hook 'post-command-hook    hookfun t))))
          (add-hook 'minibuffer-exit-hook exitfun nil t)
          (add-hook 'post-command-hook    hookfun nil t)))
    (read-from-minibuffer ">>> ")
    (grizzl-selected-result index)))


(defun unimacs/buffers ()
  (interactive)
  (let* ((pre-buffers (mapcar 'buffer-name (buffer-list)))
         (filt-buffers (delq nil (mapcar (lambda (s)
                                           (if (eq 32 (string-to-char s)) nil s))
                                         pre-buffers)))
         (index (grizzl-make-index filt-buffers))
         (newbuf (unimacs/completing-read " Unimacs: Switch to buffer" index)))
    (switch-to-buffer newbuf)))


(provide 'unimacs)
