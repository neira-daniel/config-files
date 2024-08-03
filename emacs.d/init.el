;;; init.el -*- lexical-binding: t; -*-

;; straight.el bootstrapping

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; straight.el and use-package integration

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Emacs major version and host operating system identification

(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;; Unix tools look for HOME, but this is normally not defined on Windows

(when (and IS-WINDOWS (null (getenv "HOME")))
  (setenv "HOME" (getenv "USERPROFILE")))

;; set startup default directory as $HOME

(setq inhibit-startup-message t)
(setq default-directory (file-name-as-directory (getenv "HOME")))

;; set the Emacs custom file (create one if it doesn't exist) and load it

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; start the week on Monday

(setq calendar-week-start-day 1)

;; Contrary to what many Emacs users have in their configs, you really don't
;; need more than this to make UTF-8 the default coding system:

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please

;; The clipboard's on Windows could be in a wider (or thinner) encoding than
;; utf-8 (likely UTF-16), so let Emacs/the OS decide what encoding to use there.

(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8)) ; with sugar on top

;; minibuffer completion assistant

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;; better search function

(use-package swiper
  :after (ivy)
  :config
  (setq ivy-wrap t)
  ;; :commands + :init -> :bind with conditionals
  :commands
  swiper-isearch
  :init
  (if IS-MAC
      (bind-key "s-f" 'swiper-isearch)
    (bind-key "C-f" 'swiper-isearch)))

;; change loading order preference (.elc, and then .el) to whichever file is newer

(setq load-prefer-newer t)

;; keep a list of recently opened files for easy access

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 15)
  :bind
  ("C-x C-r" . recentf-open-files))

;; configuration of undo-fu: undo and redo that make sense

(use-package undo-fu
  :after (evil)  ;; tuve que agregarlo 20200314
  :config
  (setq undo-fu-ignore-keyboard-quit t)

;;  (global-undo-tree-mode -1)
;;  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
;;  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)

  :bind
  ("s-z" . undo-fu-only-undo)
  ("s-Z" . undo-fu-only-redo)
  ("s-y" . undo-fu-only-redo))

;; disable the bell ring and the visual aid

(setq ring-bell-function #'ignore
      visible-bell nil)

;; I'm in charge with saving my work, so don't autosave, please

(setq auto-save-default nil)

;; if there's no chance that 2+ people edit the same
;; file at the same time with Emacs, there's no point
;; in having this option active, so we set it with nil

(setq create-lockfiles nil)

;; don't backup visited files

(setq make-backup-files nil)

;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.

(setq x-stretch-cursor nil)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.

(setq echo-keystrokes 0.4)

;; Save keystrokes when answering yes or no questions

(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; when dividing the screen put windows on top of each other

(setq split-width-threshold nil)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:

(setq idle-update-delay 1.0)

;; set font: working solution for emacsclient and emacs instances

(defvar nox/fonts '(("Menlo" . 18)
                    ("DejaVu Sans Mono" . 12)
                    ("Hack" . 11)
                    ("Inconsolata" . 13)
                    ("Source Code Pro" . 11))
  "List of fonts and sizes. The first one available will be used.")

(defun nox/change-font ()
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font nox/fonts (setq available-fonts (nreverse available-fonts)))
      (when (member (car font) (font-family-list))
        (push font available-fonts)))
                                        ; if: si no se encontró la fuente requerida
    (if (not available-fonts)
        (error "No fonts from the chosen set are available")
                                        ; else: sí se encontró la fuente
                                        ;   nested if: si la llamada fue interactiva
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t) available-fonts)))
            (setq font-name (car chosen)
                  font-size (read-number "Font size: " (cdr chosen))))
                                        ;    nested else: llamada no interactiva; guardar nombre/tamaño de fuente por separado
        (setq font-name (caar available-fonts)
              font-size (cdar available-fonts)))
                                        ; armar el string nombre-tamaño (por ejemplo, "Menlo-16")
      (setq font-setting (format "%s-%d" font-name font-size))
                                        ; configurar la fuente
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting)))))

(defun nox/setup-font (frame)
  (with-selected-frame frame
    (remove-hook 'after-make-frame-functions 'nox/setup-font)
    (nox/change-font)))

;; original
;;(if (daemonp)
;;    (add-hook 'after-make-frame-functions 'nox/setup-font)
;;  (nox/setup-font (car (frame-list))))
;; nuevo, pero falta corregirlo:
;; -> uno podría ejecutar Emacs en un terminal conectada
;; al daemon (en cuyo caso, if daemonp == TRUE se caerá porque
;; terminal no tiene las fuentes de las funciones NOX
(if (daemonp)
    (add-hook 'after-make-frame-functions 'nox/setup-font)
  (if (display-graphic-p)
      (nox/setup-font (car (frame-list)))))

;; show file path in the frame title (top bar in GUI)

(setq frame-title-format '((:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name))
                                    "%b"))))

;; show all the completions available for an entered command prefix

(use-package which-key
  :config
  (which-key-mode 1))

;; winner-mode: restore window configurations

(when (fboundp 'winner-mode)
  (winner-mode 1))

;; unbind right-ALT from Emacs

(when IS-MAC
  (setq ns-right-alternate-modifier 'none))

;; kill from point (the cursor) to the left with C-<backspace>

(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)))

;; don't kill a region with C-w unless it is highlighted

(defun nerfed-kill ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-begin) (region-end))))
(global-set-key (kbd "C-w") 'nerfed-kill)

;; allow to overwrite selection

(delete-selection-mode t)

;; line-wrap in all text files

(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

;; indent with spaces

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(use-package ws-butler
  :straight
  (ws-butler :type git
             :host github :repo "lewang/ws-butler"
             :fork (:host github :repo "hlissner/ws-butler"))
  :commands
  (ws-butler-mode)
  :hook
  ((prog-mode text-mode) . ws-butler-mode))

;; zoom in/out text with the keyboard

(if IS-MAC
    (progn
      (global-set-key (kbd "s-+") 'text-scale-increase)
      (global-set-key (kbd "s--") 'text-scale-decrease))
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease))

;; distraction-free modes
;; olivetti: simple solution (just margins around the text)
;; writeroom: involved solution (no modeline, no scroll bar…)

(use-package olivetti
  :init
  (setq olivetti-body-width 86))

(use-package writeroom-mode
  :config
  (setq writeroom-width 86))

;; theme: leuven

(use-package leuven-theme
  :config
  (load-theme 'leuven t))

;; https://github.com/seagle0128/doom-modeline#use-package

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (add-to-list #'yas-snippet-dirs (locate-user-emacs-file "snippets")))

;; configure the almighty org-mode

(defun deactivate-electric-indent-local-mode ()
  "Since Org 9.4, RET and C-j obey electric-indent-mode. This indentation is
fine for source code, but I find it inconvenient when writing org documents.
This function deactivates the electric-indent minor mode locally. We must load
it with the org hook."
  (electric-indent-local-mode -1))

(use-package org
  :defer t
  :config
  ;; go back to old (pre-Org 9.4) org-return behaviour (see Org 9.4 release notes)
  (add-hook 'org-mode-hook 'deactivate-electric-indent-local-mode)
  ;; default view when opening a file: all the headers, and only the headers
  (setq org-startup-folded 'content)
  ;; always leave an empty line between collapsed headers
  (setq org-cycle-separator-lines 1)
  ;; activate speed commands on headers
  (setq org-use-speed-commands t)
  ;; activate special behaviour for C-a/e and C-k on headers
;; no funciona
;;  (setq org-special-ctrl-a/e t
;;        org-special-ctrl-k t)
  ;; use a curved arrow instead of 3 dots to signal an invisible region
  (setq org-ellipsis " ⤵")
  ;; don't underline the org-ellipsis character(s)
  (set-face-attribute 'org-ellipsis nil :underline nil)
  ;; deal with edits on invisible regions in a smart way
  (setq org-catch-invisible-edits 'smart)
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
  ;; bypass org-babel confirmation for evaluating the listed languages
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (member lang '("python" "R"))))
  (setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)
  ;; tell org-babel which languages it should be aware of
  ;; nil para desactivar, t para activar
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . nil)
     (python . t)
     (emacs-lisp . t)))
;;  :bind
;;  (:map org-mode-map
;;        ("C-a" . org-beginning-of-line)
;;        ("C-e" . org-end-of-line)
;;        ("C-k" . org-kill-line))
  )

;; https://emacs.stackexchange.com/a/20093/26521
;; https://ddavis.io/posts/emacs-python-lsp/
(use-package pyvenv
  :config
  (setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs/"))
  (pyvenv-mode 1))
;; M-x pyvenv-workon
;; M-x pyvenv-deactivate

;; Org-roam: a Roam Research clone that implements a Zettelkasten

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory
   (expand-file-name
    (file-name-as-directory "~/org/Zettelkasten")))
  (org-roam-completion-system 'ivy)
  (org-roam-buffer-position 'right)
  (org-roam-buffer-width 0.33)
  :bind
  (:map org-roam-mode-map
   ("C-c n b" . org-roam)
   ("C-c n f" . org-roam-find-file-immediate)
   :map org-mode-map
   ("C-c n i" . org-roam-insert)
   ("C-c n I" . org-roam-insert-immediate)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; https://www.youtube.com/watch?v=Uz_0i27wYbg
;; https://ryan.himmelwright.net/post/emacs-update-evil-usepackage/
;; https://teddit.net/r/emacs/comments/esi403/straightel_usepackage_evil_not_running_config/
;; https://teddit.net/r/emacs/comments/726p7i/evil_mode_and_use_package/
;; https://teddit.net/r/emacs/comments/9ctvmo/evilorgmode/
;; https://github.com/Somelauw/evil-org-mode

;; evil packages/plug-ins
;; https://github.com/syl20bnr/spacemacs/blob/b6aed092cf1de8992522d69c8158a20df880a84e/layers/%2Bspacemacs/spacemacs-evil/packages.el#L12
;; https://www.emacswiki.org/emacs/Evil#h5o-6
;; https://github.com/hlissner/doom-emacs/tree/235c386368f0814671131d0d77e32be450c92cbc/modules/editor/evil#plugins

(use-package evil-leader
  :config
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "º")
  (evil-leader/set-key
   "b" 'ivy-switch-buffer
   "f" 'find-file
   "r" 'recentf-open-files
   "o" 'other-window))


;; https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))


;; https://github.com/emacs-evil/evil
(use-package evil
  :after (evil-leader)
  :init
  ;; https://evil.readthedocs.io/en/latest/settings.html
  (setq evil-vsplit-window-right t
        evil-split-window-below t
        evil-undo-system 'undo-fu) ;; no funciona
  :config
  (evil-mode 1))


;; http://vimcasts.org/episodes/swapping-two-regions-of-text-with-exchange-vim/
(use-package evil-exchange
  ;; init: spacemacs config:
  ;; https://github.com/syl20bnr/spacemacs/blob/b6aed092cf1de8992522d69c8158a20df880a84e/layers/%2Bspacemacs/spacemacs-evil/packages.el#L134
  :init
  (progn
    (let ((evil-exchange-key (kbd "gx"))
          (evil-exchange-cancel-key (kbd "gX")))
      (define-key evil-normal-state-map evil-exchange-key 'evil-exchange)
      (define-key evil-visual-state-map evil-exchange-key 'evil-exchange)
      (define-key evil-normal-state-map evil-exchange-cancel-key 'evil-exchange-cancel)
      (define-key evil-visual-state-map evil-exchange-cancel-key 'evil-exchange-cancel))))


;; https://github.com/Somelauw/evil-org-mode/
(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  ;; navigation: https://github.com/Somelauw/evil-org-mode/blob/a629fb705b0ac704580d5a5833a64716284074e7/evil-org.el#L680
  (evil-org-set-key-theme '(textobjects additional todo)))

(use-package evil-unimpaired
  :straight
  (evil-unimpaired :type git
                   :host github :repo "zmaas/evil-unimpaired")
  :config
  (evil-unimpaired-mode))

;; para separar los portapapeles: Emacs/Evil y el sistema manejan
;; registros distintos
;; así, cmd-c/x/v trabajan por separado de los yank y put de Emacs/Evil
(use-package simpleclip
  :config
  (simpleclip-mode 1))

;; para aumentar o disminuir un contador
;; 2022-07-26
(use-package evil-numbers
  :config
  (evil-define-key '(normal visual) 'global (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C-c -") 'evil-numbers/dec-at-pt))


;; considerar instalar después
;; https://github.com/cute-jumper/evil-embrace.el
;; https://github.com/syl20bnr/evil-escape
;; https://github.com/edkolev/evil-lion
;; https://github.com/redguardtoo/evil-nerd-commenter
;; https://github.com/hlissner/evil-snipe
