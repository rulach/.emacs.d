
;; Mode-line format
mode-line-format
(setq-default mode-line-format
	      '(;; Only read and modified file
		" %2 "
		(:eval
		 (cond (buffer-read-only
			(propertize " RO " 'face 'mode-line-read-only-face))
		       ((buffer-modified-p)
			(propertize " ** " 'face 'mode-line-modified-face))
		       ))

		;; Folder and file
		" %1 "
		(:propertize (:eval (shorten-directory default-directory 30))
			     face mode-line-folder-face)
		(:propertize "%b"
			     face mode-line-filename-face)

		;; Line and column
		" %2 [%l:%p]"

		;; Edition mode
		" %2 "
		(:propertize " %m " face mode-line-mode-face)

		;; Date and time
		" %2 "
		(global-mode-string global-mode-string)
		))

;; Function that extracts the folder of the file
(defun shorten-directory (dir max-length)
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
	(output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Colours for each section of mode-line
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-mode-face)

(set-face-attribute 'mode-line nil
    :foreground "gray80" :background "#326252"
    :box '(:color "#326252" :style nil))

(set-face-attribute 'mode-line-inactive nil
    :foreground "gray80" :background "gray40"
    :box '(:color "gray40" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 :color "#4271ae"))

(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :background "#ffffff"
    :box '(:line-width 2 :color "#c82829"))

(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "grey90")

(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)

(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray90" :background "#2E8166")

(provide 'modeline)
