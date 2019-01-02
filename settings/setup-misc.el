
;; Change "yes" to "y" and "no" to "n"
(fset 'yes-or-no-p 'y-or-n-p)

;; Move to trash deleted files and folders
(setq delete-by-moving-to-trash t
trash-directory "~/.local/share/Trash/files")

;; Save session when closing emacs (0 to disable)
(desktop-save-mode 0)

;; No backup and no autosave
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Open folders when C-x b (ido)
(ido-mode 1)

;; Ignoring some buffers.
(setq ido-ignore-buffers '("^ " "*Completions*"
                           "*Shell Command Output*"
                           "*Messages*" "Async Shell Command"
                           "*scratch*" "*tramp*"))

;; Delete blanks in first and last lines, and in the final of all lines
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'write-file-hooks 'delete-trailing-whitespace nil t)

(provide 'setup-misc)
