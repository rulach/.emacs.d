
;; Unmap C-z
(global-unset-key (kbd "C-z"))

;; Ansi-term with bash in F5
(global-set-key (kbd "<f5>") '(lambda ()(interactive)(ansi-term "/bin/bash")))

;; Windows movements
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; Windows size adjustment
(global-set-key (kbd "M-<up>") 'enlarge-window)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)

;; Deleting words in "-nw" mode
(global-set-key [(control ?h)] 'backward-kill-word)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Screen scroll
(setq scroll-preserve-screen-position 1)
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(setq next-line-add-newlines t)

;; Redo and undo
(global-set-key (kbd "C-z") (kbd "C-_"))
(global-set-key (kbd "M-z") (kbd "M-_"))

;; Comments
(global-set-key (kbd "C--") 'comment-line)
(global-set-key (kbd "M--") 'comment-line)

(provide 'setup-key-bindings)
