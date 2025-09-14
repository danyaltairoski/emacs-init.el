;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Core Settings
;;------------------------------------------------------------------------------

(setq inhibit-startup-message t)          ; no startup screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))   ;; maximized
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
;; UI Tweaks
;;------------------------------------------------------------------------------

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(set-face-attribute 'default nil :height 175)

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
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;------------------------------------------------------------------------------
;; Appearance (Themes & Modeline)
;;------------------------------------------------------------------------------

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-day t))

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
      (load-theme (if dark-mode 'doom-tomorrow-night 'doom-tomorrow-day) t)
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
;; Programming Languages
;;------------------------------------------------------------------------------

;; Assembly
(add-to-list 'auto-mode-alist '("\\.asm\\'" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.s\\'"   . asm-mode))
(setq asm-comment-char ?#) ;; or ?\; for NASM

(use-package riscv-mode :mode "\\.s\\'")

;;------------------------------------------------------------------------------
;; Convenience
;;------------------------------------------------------------------------------

(defun open-init-file ()
  "Open my Emacs init file quickly."
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c I") 'open-init-file)

