
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
(require 'key-bindings)
(require 'modeline)


;; Elimina el mensaje de bienvenida
(package-initialize)
(setq inhibit-startup-message t)

;; Elimina los menus y la barra de scroll
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


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

;; Añadir un espacio entre el número de linea y el texto de la
;; linea al lanzar M-x linum-mode (ver números de linea)
(global-linum-mode t)
(setq linum-format "%d ")

;; Mostrar fecha y hora en formato 24 horas:
(setq display-time-day-and-date t display-time-24hr-format t)
(display-time)

;; Carga el repositorio de MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t))

;; Funcion de instalacion de paquetes
(defun require-package (package &optional min-version no-refresh)
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;; Comentarios
(global-set-key (kbd "C--") 'comment-line)
(global-set-key (kbd "M--") 'comment-line)

;; Parentesis
(show-paren-mode t)

;; Tema de emacs
(require-package 'material-theme)
(load-theme 'material t)

;; Tamano de la fuente
(set-face-attribute 'default nil :height 100)

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
