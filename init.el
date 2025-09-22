;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Core Settings
;;------------------------------------------------------------------------------

(setq inhibit-startup-message t)          ; no startup screen
(setq confirm-kill-emacs 'y-or-n-p)       ; ask before exiting
(setq default-directory "~/Desktop/")     ; default C-x C-f dir
(setq auto-save-default nil)              ; don't auto-save
(setq make-backup-files nil)              ; don't make backups

;; Backups & Autosaves
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))

;; Store customize settings separately
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;------------------------------------------------------------------------------
;; Ensure Emacs sees your shell PATH (macOS only)
;;------------------------------------------------------------------------------
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;------------------------------------------------------------------------------
;; UI Tweaks
;;------------------------------------------------------------------------------

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(set-face-attribute 'default nil :height 160)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(delete-selection-mode 1) ; typing replaces selection

;;------------------------------------------------------------------------------
;; Package System Setup
;;------------------------------------------------------------------------------

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;------------------------------------------------------------------------------
;; Appearance (Themes & Modeline)
;;------------------------------------------------------------------------------

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-themes
  :config
  (load-theme 'doom-one-light t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Auto theme switching on macOS
(defvar my/macos-last-dark-mode nil "Last detected macOS dark mode state.")

(defun my/macos-dark-mode-p ()
  "Return t if macOS is in dark mode."
  (string= "true"
           (string-trim
            (shell-command-to-string
             "osascript -e 'tell application \"System Events\" \
to tell appearance preferences to get dark mode' 2>/dev/null"))))

(defun my/macos-theme-sync ()
  "Load Doom theme based on macOS dark/light appearance."
  (let ((dark-mode (my/macos-dark-mode-p)))
    (unless (eq dark-mode my/macos-last-dark-mode)
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme (if dark-mode 'doom-one 'doom-one-light) t)
      (setq my/macos-last-dark-mode dark-mode))))

(my/macos-theme-sync)
(run-with-timer 0 10 #'my/macos-theme-sync)

;;------------------------------------------------------------------------------
;; Completion & Navigation
;;------------------------------------------------------------------------------

(use-package ivy
  :diminish
  :config (ivy-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x"     . counsel-M-x)
         ("C-x b"   . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r"     . counsel-minibuffer-history)))

(defun my/ivy-switch-buffer-only ()
  "Switch to existing buffer only, no creation."
  (interactive)
  (ivy-read "Switch to buffer: "
            (mapcar #'buffer-name (buffer-list))
            :action #'switch-to-buffer
            :require-match t))

(global-set-key (kbd "C-x b") #'my/ivy-switch-buffer-only)

(use-package which-key
  :diminish
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.3))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;------------------------------------------------------------------------------
;; Org Mode
;;------------------------------------------------------------------------------

(use-package org
  :config
  (setq org-directory "~/Desktop/org")
  (setq org-agenda-files '("~/Desktop/org/todo.org"
                           "~/Desktop/org/school.org"
                           "~/Desktop/org/calendar.org"
                           "~/Desktop/org/dailyplan.org"))
  (setq org-startup-indented t
        org-hide-leading-stars t
        org-pretty-entities t
        org-startup-folded t
        org-cycle-separator-lines 0
        org-insert-heading-respect-content t
        org-M-RET-may-split-line '((default . t))
        org-list-allow-alphabetical t
        org-agenda-window-setup 'current-window)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-capture-templates
        '(("c" "Capture" entry (file "~/Desktop/org/refile.org")
           "* %?\n  %U\n  %a")))

  (setq org-agenda-custom-commands
        '(("d" "Dailyplan TODOs"
           todo ""
           ((org-agenda-files '("~/Desktop/org/dailyplan.org"))))))

  (global-set-key (kbd "C-c n a") 'org-agenda)
  (global-set-key (kbd "C-c n n") 'org-capture)

  ;; Smarter movement and editing
  (with-eval-after-load 'org
    (setq org-special-ctrl-a/e t)
    (define-key org-mode-map (kbd "M-<up>")    #'org-metaup)
    (define-key org-mode-map (kbd "M-<down>")  #'org-metadown)
    (define-key org-mode-map (kbd "M-<left>")  #'org-metaleft)
    (define-key org-mode-map (kbd "M-<right>") #'org-metaright)
    (define-key org-mode-map (kbd "C-<return>") #'my/org-C-RET-smart)))

;; Org helper functions
(defun my/org-birthday-agenda ()
  "Open agenda restricted to birthdays."
  (interactive)
  (org-agenda nil "b"))

(defun my/org-fontify-drawer-lists (limit)
  "Fontify list markers inside Org drawers."
  (while (re-search-forward
          "^[ \t]*\\([-+]\\|[0-9]+[.)]\\)[ \t]+" limit t)
    (add-text-properties (match-beginning 1) (match-end 1)
                         '(face font-lock-keyword-face))))

(font-lock-add-keywords 'org-mode '((my/org-fontify-drawer-lists)))

(defun my/org-C-RET-smart (&optional arg)
  "Smart C-RET: insert list bullet or heading with TODO carryover."
  (interactive "P")
  (if (org-in-item-p)
      (progn (end-of-line) (org-insert-item) (end-of-line))
    (let ((prev-todo (org-get-todo-state)))
      (org-insert-heading-respect-content arg)
      (when prev-todo (org-todo prev-todo)))))


(setq org-refile-targets
      '(("~/Desktop/org/todo.org"      :maxlevel . 2)
        ("~/Desktop/org/resources.org" :maxlevel . 2)
        ("~/Desktop/org/shopping.org"  :maxlevel . 1)))



;;------------------------------------------------------------------------------
;; Convenience
;;------------------------------------------------------------------------------

(defun open-init-file ()
  "Open my Emacs init file quickly."
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c I") 'open-init-file)

;;------------------------------------------------------------------------------
;; Projectile - project management
;;------------------------------------------------------------------------------

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode 1)
  (setq projectile-project-search-path '("~/Desktop/projects"))
  (setq projectile-switch-project-action #'projectile-dired)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :after (projectile counsel)
  :config (counsel-projectile-mode 1))

;;------------------------------------------------------------------------------
;; Magit - Git interface
;;------------------------------------------------------------------------------
(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :bind
  ("C-x g" . magit-status))  ;; common shortcut to open Magit

;;------------------------------------------------------------------------------
;; LSP Mode (Language Server Protocol)
;;------------------------------------------------------------------------------
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (latex-mode . lsp-deferred)
         (html-mode . lsp-deferred)
         (css-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; keymap for lsp commands
  :config
  (lsp-enable-which-key-integration t))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; Optional: UI enhancements for LSP ------------------------------
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; Treemacs
(use-package treemacs
  :ensure t)

;; VTerm
(use-package vterm
  :ensure t)

(use-package lsp-ivy)

;; Which-key integration (helps you discover keybindings)
(use-package which-key
  :config
  (which-key-mode))

;; Company mode for autocompletion --------------------------------
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;;------------------------------------------------------------------------------
;; Programming Languages
;;------------------------------------------------------------------------------

;; Assembly
(add-to-list 'auto-mode-alist '("\\.asm\\'" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.s\\'"   . asm-mode))
(setq asm-comment-char ?#) ;; or ?\; for NASM
(setq lsp-asm-lsp-executable "asm-lsp")

(add-hook 'asm-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode nil)))

;; Web Development
;; Use web-mode for HTML, CSS, and JS files
(use-package web-mode
  :mode ("\\.html?\\'" "\\.css\\'" "\\.js\\'")
  :hook (web-mode . lsp-deferred)
  :config
  ;; indentation settings
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-current-element-highlight t)) ;; highlights current tag)

;; Emmet-mode for fast HTML/CSS expansions
(use-package emmet-mode
  :hook (web-mode css-mode html-mode))

;; Prettier for auto-formatting JS/TS/CSS/HTML
(use-package prettier-js
  :hook ((js-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (css-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

;;-----------
;; PDF Support
;;-------------

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))))   

(add-hook 'pdf-view-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))

(setq insert-directory-program "gls")

(defun open-in-firefox ()
  "Open the current file in Firefox on macOS."
  (interactive)
  (when buffer-file-name
    (save-buffer) ;; save before opening
    (start-process "firefox" nil "open" "-a" "Firefox" buffer-file-name)))

(global-set-key (kbd "C-c f") 'open-in-firefox)

;; remove annoying lsp snippet
(setq lsp-auto-guess-root t)
(setq lsp-session-file (expand-file-name ".lsp-session-v1" user-emacs-directory))
(setq lsp-enable-snippet nil) ;; optional

(setq TeX-PDF-mode t)        ;; compile directly to PDF
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil) ;; query for master file
