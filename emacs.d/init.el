;; Inicialización o actualización de variables
;;   - nombre de usuario
(setq user-full-name "Daniel Neira")
;;   - directorio de inicio (el que aparecerá al presionar C-x C-f por primera vez)
;;     - debe estar ubicado en el directorio personal del usuario
;;       - C:\Users\{username} en Windows
;;       - ${HOME} en los demás sistemas operativos
;;     - si no existe, será creado
(setq dn/starting-folder "org-vault")

;; función auxiliar
(defun dn/string-match-p (REGEXP STRING)
  " Wrapper para string-match-p que devuelve t cuando
hay coincidencia y nil cuando no."
  (if (string-match-p REGEXP STRING)
      t
    nil))

;; obtención del sistema operativo que aloja Emacs
;; (defconst IS-WSL     (and (eq system-type 'gnu/linux)
;; (dn/string-match-p "Microsoft"
;; (shell-command-to-string "uname -a"))))
;; (defconst IS-LINUX   (and (not IS-WSL)
;; (eq system-type 'gnu/linux)))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-LINUX   (eq system-type 'gnu/linux))
;; asumimos que trabajamos con, al menos, Windows Vista
(defconst ON-WINDOWS (eq system-type 'windows-nt))
(defconst ON-WSL     (and (string-match-p "Microsoft" operating-system-release)
			  ON-LINUX))

;; obtención de la versión de Emacs
(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst EMACS29+   (> emacs-major-version 28))

;;; init.el -*- coding: utf-8-unix; -*- lexical-binding: t; -*-

;; bootstraping straight.el de acuerdo al manual
;; ver GH·radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; instalación de use-package
(straight-use-package 'use-package)

;; configuramos use-package para que siempre instale paquetes con straight.el
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; configuración de Evil
(use-package evil
  :init
  ;; https://evil.readthedocs.io/en/latest/settings.html
  (setq evil-vsplit-window-right t
	evil-split-window-below t
	evil-undo-system 'undo-fu)
  ;; desactivamos C-i para que TAB funcione en la terminal
  (setq evil-want-C-i-jump nil)
  :config
  ;; definición de la tecla <leader>
  (evil-set-leader 'normal (kbd "SPC"))
  ;; guardar el archivo con w
  (evil-define-key 'normal 'global (kbd "<leader>w") 'save-buffer)
  ;; utilizar U para deshacer un cambio (notemos que U cumple otra función en Vim y aquí la estamos sobreeescribiendo)
  (evil-define-key 'normal 'global (kbd "U") 'evil-redo)
  ;; borrar desde el cursor hasta el comienzo de la línea sin modificar los registros
  (evil-define-key 'normal 'global (kbd "C-<backspace>") (kbd "\"_d0"))
  (evil-define-key 'insert 'global (kbd "C-<backspace>") (kbd "C-o \" _ d 0"))
  ;; ir al siguiente buffer
  (evil-define-key 'normal 'global (kbd "<leader>o") (kbd "C-x o"))
  ;; activar evil-mode
  (evil-mode 1))

;; configuración de evil-org
(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(textobjects additional return todo)))

;; configuración de evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; configuración de evil-exchange
(use-package evil-exchange
  :init
  (let ((evil-exchange-key (kbd "gx"))
      (evil-exchange-cancel-key (kbd "gX")))
    (define-key evil-normal-state-map evil-exchange-key 'evil-exchange)
    (define-key evil-visual-state-map evil-exchange-key 'evil-exchange)
    (define-key evil-normal-state-map evil-exchange-cancel-key 'evil-exchange-cancel)
    (define-key evil-visual-state-map evil-exchange-cancel-key 'evil-exchange-cancel)))

;; configuración de evil-unimpaired
(use-package evil-unimpaired
  :straight
  (evil-unimpaired :type git
                   :host github :repo "zmaas/evil-unimpaired")
  :config
  (evil-unimpaired-mode))

;; configuración de evil-terminal-cursor-changer
(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
    :config
    (evil-terminal-cursor-changer-activate)))

;; configuración de evil-owl
(use-package evil-owl
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode))

;; configuración de undo-fu
(use-package undo-fu
  :after (evil)
  :config
  (setq undo-fu-ignore-keyboard-quit t)
  (global-set-key (kbd "M-z")   'undo-fu-only-undo)
  (global-set-key (kbd "M-S-z") 'undo-fu-only-redo))

;; configuración de yasnippet
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (add-to-list #'yas-snippet-dirs (locate-user-emacs-file "snippets")))

;; configuración de Ivy
(use-package ivy
  :config
  (ivy-mode 1)
  ;;(setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;; configuración de which-key
(use-package which-key
  :config
  (setq echo-keystrokes 0.4)
  (which-key-mode 1))

;; configuración de recentf
(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 15)
  :bind
  ("C-c r" . recentf-open-files))

;; configuración de emacs-leuven-theme
(use-package leuven-theme
  ;; :config
  ;; (load-theme 'leuven t)
  )

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-city-lights t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package solaire-mode
  :after (doom-themes))

(use-package ample-theme
  :init
  (load-theme 'ample t t)
  (load-theme 'ample-flat t t)
  (load-theme 'ample-light t t)
  ;; (enable-theme 'ample-flat)
  )

;; función auxiliar
(defun activate-olivetti-mode ()
  (olivetti-mode 1))
;; configuración de olivetti
(use-package olivetti
  :init
  (setq olivetti-body-width 86)
  :hook
  (text-mode . activate-olivetti-mode))

;; configuración de ws-butler
(use-package ws-butler
  :straight
  (ws-butler :type git
             :host github :repo "lewang/ws-butler"
             :fork (:host github :repo "hlissner/ws-butler"))
  :commands
  ;; ¿será necesario activarlo si luego lo engancho con hook?
  (ws-butler-mode)
  :hook
  ((prog-mode text-mode) . ws-butler-mode))

;; - actualizar propiedad de documento Org
(defun dn/update-org-property (property-regex property-new-value)
  " Replaces first match of property-regex value with property-new-value.
It only works in Org-mode."
  (when (eq major-mode 'org-mode)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward property-regex (point-max) t)
        (progn
          ;;(kill-line) guardaría la timestamp en el kill-ring
	  ;;(delete-region a b) no lo hace
	  ;; https://stackoverflow.com/a/21780995
	  ;; alt: https://unix.stackexchange.com/a/136581
	  (delete-region (point) (line-end-position))
          (insert (concat " " property-new-value))
          )))))

(defun dn/update-org-last-modified ()
  " Updates the value of LAST_MODIFIED with current timestamp."
  (interactive)
  ;; desactivamos undo de manera momentánea
  ;; fuente: https://emacs.stackexchange.com/a/4222
  (let (buffer-undo-list)
    (dn/update-org-property "^#\\+LAST_MODIFIED:"
			    (format-time-string "%Y-%m-%dT%H:%M:%S%:z"))))

;; borrar enlace de documento Org
;; fuente: https://emacs.stackexchange.com/a/10714
(defun afs/org-replace-link-by-link-description ()
  "Replace an Org link by its description or, if empty, its address."
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2)
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))

;; configuración de Org-mode
(use-package org
  :defer t
  :config
  ;; go back to old (pre-Org 9.4) org-return behaviour (see Org 9.4 release notes)
  ;;(add-hook 'org-mode-hook 'deactivate-electric-indent-local-mode)
  ;; Org initial visibility: global settings
  ;;   local +STARTUP alternatives:
  ;;     'overview', 'content', 'showall', 'show2levels', . . ., 'show5levels', 'showeverything'
  (setq org-startup-folded 'content)
  ;; Org indent mode: global settings
  ;;   local +STARTUP alternatives: 'indent' and 'noindent'
  ;;   note that +STARTUP 'indent' leaves 'hidestars' redundant and has precedence over 'showstars'
  (setq org-startup-indented t)
  ;; always leave an empty line between collapsed headers
  (setq org-cycle-separator-lines 1)
  ;; activate speed commands on headers
  ;; also: special shortcuts on headers and lists
  ;; (note that all these work in Insert "mode" only)
  (setq org-use-speed-commands t
	org-special-ctrl-a/e t
	org-special-ctrl-k t
	org-ctrl-k-protect-subtree 'error)
  ;; use a curved arrow instead of 3 dots to signal an invisible region
  (setq org-ellipsis " ⤵")
  ;; don't underline the org-ellipsis character(s)
  (set-face-attribute 'org-ellipsis nil :underline nil)
  ;; deal with edits on invisible regions in a smart way
  (setq org-catch-invisible-edits 'show-and-error)
  ;; never hide the emphasis markers (for example, slashes for italics)
  (setq org-hide-emphasis-markers nil)
  ;; how to display LaTeX images (math)
  ;; C-c C-x C-l (org-latex-preview) <-- show/hide equation next to point
  ;; C-u C-c C-x C-l <-- show/hide equations in current section
  ;; C-u C-u C-c C-x C-l <-- show/hide equations in the current document
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8)
	org-format-latex-options (plist-put org-format-latex-options :foreground "Black")
	org-format-latex-options (plist-put org-format-latex-options :background "White")
	org-format-latex-options (plist-put org-format-latex-options :html-foreground "Black")
	org-format-latex-options (plist-put org-format-latex-options :html-background "Transparent")
	org-format-latex-options (plist-put org-format-latex-options :html-scale 1.0))
  ;; bypass org-babel confirmation when evaluating the listed languages
  (defun ryuslash/org-confirm-babel-evaluate (lang body)
    (not (member lang '("R"))))
  (setq org-confirm-babel-evaluate #'ryuslash/org-confirm-babel-evaluate)
  ;; tell org-babel which languages it should be aware of
  ;; nil para desactivar, t para activar
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . nil)
     (emacs-lisp . t)))
  ;;  :bind
  ;;  (:map org-mode-map
  ;;        ("C-a" . org-beginning-of-line)
  ;;        ("C-e" . org-end-of-line)
  ;;        ("C-k" . org-kill-line))
  ;;(put 'org-mode 'flyspell-mode-predicate 'org-mode-flyspell-verify)
  ;; reloj de Org mode
  (evil-define-key 'normal 'global (kbd "<leader>ci") 'org-clock-in)
  (evil-define-key 'normal 'global (kbd "<leader>co") 'org-clock-out)
  (evil-define-key 'normal 'global (kbd "<leader>cl") 'org-clock-in-last)
  (evil-define-key 'normal 'global (kbd "<leader>cs") 'org-clock-display)
  :hook
  ;;(org-mode . flyspell-mode)
  (before-save . dn/update-org-last-modified))

(setq inhibit-startup-message t)

;;; Encodings
;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment 'utf-8)
;; ...but `set-language-environment' also sets `default-input-method', which is
;; a step too opinionated.
;; (setq default-input-method nil)
;; ...And the clipboard on Windows could be in a wider encoding (UTF-16), so
;; leave Emacs to its own devices there.
;;(eval-when! (not doom--system-windows-p)
;;  (setq selection-coding-system 'utf-8))
(setq selection-coding-system 'utf-8)

;; Emacs en Windows puede comportarse distinto en cuanto a los saltos de línea
;; buscamos que prefiera utf8 con LF de unix
;; https://emacs.stackexchange.com/a/75782
(prefer-coding-system 'utf-8-unix)
;;(setq coding-system-for-read 'utf-8-unix)
;;(setq coding-system-for-write 'utf-8-unix)

;; http://xahlee.info/emacs/emacs/emacs_file_encoding.html
;; http://xahlee.info/emacs/emacs/emacs_encoding_decoding_faq.html
;;(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
;; add this especially on Windows, else python might show output problems
(set-terminal-coding-system 'utf-8-unix)

;; desactivar el guardado automático de archivos
;; quienes no acostumbren guardar su trabajo periódicamente preferirán
;; no cambiar el valor de esta variable
(setq auto-save-default nil)

;; desactivar la protección contra colisiones
;; estos son los archivos con los caracteres ".#" en su nombre
(setq create-lockfiles nil)

;; desactivar la creación de archivos de respaldo
;; estos son los archivos con caracter "~" al final de su nombre
(setq make-backup-files nil)

;; configurar el directorio de inicio
;;   función auxiliar para subir en la jerarquía de directorios
(defun cb/parent-directory (dir)
  " Ruta al directorio padre de `dir'.
Fuente: https://stackoverflow.com/a/14096693"
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))
;;   asignación de `dn/my-home' dependiendo del sistema operativo anfitrión
(if ON-WINDOWS
    (setq dn/my-home (cb/parent-directory (cb/parent-directory (getenv "HOME"))))
  (setq dn/my-home (file-name-as-directory (getenv "HOME"))))
;;   asignación del directorio de inicio (si no existe, lo creamos)
(let ((temp-dir (expand-file-name
		 (file-name-as-directory dn/starting-folder)
		 (file-name-as-directory dn/my-home))))
  (unless (file-directory-p temp-dir)
    (make-directory temp-dir))
  (setq default-directory temp-dir))

;; fijamos el archivo secundario de configuraciones y, si existe, lo cargamos
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; desactivar la campana
(setq ring-bell-function #'ignore
      visible-bell nil)

;; activar el ajuste de línea en buffers de texto
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

;; desactivar el centrado automático de la pantalla
(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered. The default (0) triggers this too
      ;; aggressively. Setting it to 10 will trigger recenter if scrolling too far
      ;; off-screen.
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

;; activar winner-mode
(winner-mode 1)

;; don't kill a region with C-w unless it is highlighted
(defun nerfed-kill ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-begin) (region-end))))
(global-set-key (kbd "C-w") 'nerfed-kill)

;; unbind right-ALT from Emacs
(when ON-MAC
  (setq ns-right-alternate-modifier 'none))

;; ocultar la barra de herramientas de la GUI
(tool-bar-mode -1)

;; zoom in/out text with the keyboard
(if ON-MAC
    (progn
      (global-set-key (kbd "s-+") 'text-scale-increase)
      (global-set-key (kbd "s--") 'text-scale-decrease))
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease))

;; (defun dn/load-theme-cond ()
;;   "Carga un tema claro u oscuro dependiendo de si estamos ejecutando
;; Emacs con la GUI o en una terminal."
(if (display-graphic-p)
    (load-theme 'leuven t)
  (progn
    (load-theme 'doom-city-lights t)
    (solaire-global-mode +1)))

;; (add-hook 'after-make-frame-functions #'dn/load-theme-cond)

;; configurar las fuentes
;; configuramos una fuente para todo Emacs y luego configuramos los buffers
;; de texto de manera particular

;; enfoques:
;;   https://www.reddit.com/r/emacs/comments/111bsc9/comment/j8eukes/
;;   https://emacs.stackexchange.com/a/3044
;;   https://zzamboni.org/post/beautifying-org-mode-in-emacs/

(defvar dn/org-text-fonts '(("JetBrains Mono" . 12)  ;; multiplataforma
			    ("Roboto Mono" . 12) ;; multiplataforma
			    ("Menlo" . 18)  ;; exclusiva de macOS
			    ("Consolas" . 16)  ;; exclusiva de Windows
			    ("DejaVu Sans Mono" . 12))  ;; usualmente disponible en Linux
  "Tipografías deseadas y su tamaño en puntos en orden de preferencia.")

(defvar dn/org-block-fonts '(("Iosevka Fixed" . 13)  ;; multiplataforma
  			     ("Fira Code" . 12))  ;; multiplataforma
  "Tipografías monoespaciadas deseadas y su tamaño en puntos en orden de preferencia.")

(defun font-available-p (font-name)
  ;; fuente:
  ;;   https://emacsredux.com/blog/2021/12/22/check-if-a-font-is-available-with-emacs-lisp/
  (find-font (font-spec :name font-name)))

(defun dn/check-available-fonts (font-alist)
  "Verifica qué fuentes están disponibles en la plataforma anfitriona.
  Devuelve aquellas existentes en orden de preferencia."
  (let (available-fonts)
    (dolist (font font-alist)
      (when (font-available-p (car font))
        (push font available-fonts)))
    (nreverse available-fonts)))

(setq dn/available-org-text-fonts (dn/check-available-fonts dn/org-text-fonts))
(setq dn/available-org-block-fonts (dn/check-available-fonts dn/org-block-fonts))

(unless (eq dn/available-org-block-fonts nil)

  ;; (setq my-org-block-font-spec (format "%s-%d"
  ;; (caar dn/available-org-block-fonts)
  ;; (cdar dn/available-org-block-fonts)))

  (set-face-attribute 'default nil
		      :family (caar dn/available-org-block-fonts)
		      :height (* 10 (cdar dn/available-org-block-fonts)))
  )


;; (setq my-org-text-font-spec (format "%s-%d"
;; (caar dn/available-org-text-fonts)
;; (cdar dn/available-org-text-fonts)))

(defun dn/set-org-faces ()
  "Configurar la tipografía para Org."
  (with-eval-after-load 'org-faces
    (unless (eq dn/available-org-text-fonts nil)
      (face-remap-add-relative 'default
			       :family (caar dn/available-org-text-fonts)
			       :height (* 10 (cdar dn/available-org-text-fonts))))
    (unless (eq dn/available-org-block-fonts nil)
      (set-face-attribute 'org-block nil
			  :family (caar dn/available-org-block-fonts)
			  :height (* 10 (cdar dn/available-org-block-fonts))))
    ))

(add-hook 'text-mode-hook #'dn/set-org-faces)

;; función para insertar timestamp actual
(defun dn/current-timestamp ()
  " Inserta la fecha y hora actual en formato ISO 8601 con T como
separador y sin especificar microsegundos. "
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S%:z")))
