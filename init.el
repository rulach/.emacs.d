;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fichero de configuracion de emacs ;;
;;				     ;;
;; Creado por: rulach                ;;
;; Fecha creacion: 1/1/2019          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Elimina el mensaje de bienvenida
(package-initialize)
(setq inhibit-startup-message t)

;; Elimina los menus y la barra de scroll
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Desmapea el C-z
(global-unset-key (kbd "C-z"))

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

;; Carpetas adicionales del path
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

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

;; Abre terminal ansi-term con bash al pulsar F5
(global-set-key (kbd "<f5>") '(lambda ()(interactive)(ansi-term "/bin/bash")))


;; Movimiento entre ventanas y ajuste de tamaño de estas
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(global-set-key (kbd "M-<up>") 'enlarge-window)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)

;; ;; Cambiar de buffer
;; (global-set-key (kbd "<backtab>") 'next-buffer)

;; Borrado de palabras (ajuste para modo terminal -nw)
(global-set-key [(control ?h)] 'backward-kill-word)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Scroll de la pantalla
(setq scroll-preserve-screen-position 1)
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(setq next-line-add-newlines t)


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
(global-set-key (kbd "C-z") (kbd "C-_"))
(global-set-key (kbd "M-z") (kbd "M-_"))

;; Coloreado de la linea actual
(global-hl-line-mode 1)
;; (set-face-background 'hl-line "#2E8769")
(set-face-foreground 'hl-line nil)
(set-face-underline  'hl-line t)

;; Emacs transparente en modo terminal
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;; Formato de mode-line
mode-line-format
(setq-default mode-line-format
	      '(;; Solo escritura y fichero modificado
		" %2 "
		(:eval
		 (cond (buffer-read-only
			(propertize " RO " 'face 'mode-line-read-only-face))
		       ((buffer-modified-p)
			(propertize " ** " 'face 'mode-line-modified-face))
		       ))

		;; Directorio y fichero
		" %1 "
		(:propertize (:eval (shorten-directory default-directory 30))
			     face mode-line-folder-face)
		(:propertize "%b"
			     face mode-line-filename-face)

		;; Linea y columna
		" %2 [%l:%p]"

		;; Modo de edicion
		" %2 "
		(:propertize " %m " face mode-line-mode-face)

		;; Fecha y hora
		" %2 "
		(global-mode-string global-mode-string)
		))

;; Funcion para la carpeta del path del fichero
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
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

;; Colores de cada bloque del mode-line
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
