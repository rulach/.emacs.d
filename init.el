
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

;; Load settings files
(require 'melpa)
(require 'key-bindings)
(require 'modeline)
(require 'appearance)





;; Cambia "yes" y "no" por "y" y por "n"
(fset 'yes-or-no-p 'y-or-n-p)

;; Mover a la papelera al borrar archivos y directorios
(setq delete-by-moving-to-trash t
trash-directory "~/.local/share/Trash/files")

;; Guardar la sessión al cerrar emacs y restaurarla
;; al arrancar-la de nuevo. Cero (0) para desactivar:
(desktop-save-mode 0)

;; No hacer backup ni autoguardado
(setq make-backup-files nil)
(setq auto-save-default nil)



;; Buffers abiertos al pulsar C-x b (ido)
(ido-mode 1)

;; Ignorar determinados buffers.
(setq ido-ignore-buffers '("^ " "*Completions*"
                           "*Shell Command Output*"
                           "*Messages*" "Async Shell Command"
                           "*scratch*" "*tramp*"))


;; Eliminar espacios en blanco al final de la linea y al final
;; del texto de forma automática al guardar el archivo
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'write-file-hooks 'delete-trailing-whitespace nil t)



;; Comentarios
(global-set-key (kbd "C--") 'comment-line)
(global-set-key (kbd "M--") 'comment-line)




;; Redo y undo
(require-package 'undo-tree)
(global-undo-tree-mode)

;; Coloreado de la linea actual
(global-hl-line-mode 1)
(set-face-foreground 'hl-line nil)
(set-face-underline  'hl-line t)

;; Emacs transparente en modo terminal
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)



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
