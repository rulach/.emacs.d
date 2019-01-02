
;; No welcome message
(package-initialize)
(setq inhibit-startup-message t)

;; No menus and no scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Line number format
(global-linum-mode t)
(setq linum-format "%d ")

;; 24h time format
(setq display-time-day-and-date t display-time-24hr-format t)
(display-time)

;; Parenthesis
(show-paren-mode t)

;; Emacs theme
(require-package 'material-theme)
(load-theme 'material t)

;; Fonts size
(set-face-attribute 'default nil :height 100)

(provide 'appearance)
