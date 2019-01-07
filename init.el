
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Created by: Raúl Carretero (rulach)  ;;
;; E-mail: raul@rcarretero.com          ;;
;; Creation date: 1/1/2019              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add files and folders to path
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "settings/" user-emacs-directory))

(setq settings-file (expand-file-name "settings.el" user-emacs-directory))
(load settings-file)

;; Load setting files
(require 'setup-melpa)
(require 'setup-misc)
(require 'setup-key-bindings)
(require 'setup-appearance)
(require 'setup-mode-line)

;; NO TOCAR A PARTIR DE AQUI
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (dracula-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
