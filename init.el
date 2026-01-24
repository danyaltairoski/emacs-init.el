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
(set-face-attribute 'default nil
  :family "Menlo"
  :height 160)

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
  (setq org-agenda-files '("~/Desktop/org/04-areas.org"
                           "~/Desktop/org/07-school.org"
                           "~/Desktop/org/09-calendar.org"
                           "~/Desktop/org/01-today.org"))
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
      '(("c" "Capture" entry
         (file "~/Desktop/org/00-refile.org")
         "* %?\n%U\n%a")))


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
  "Smart C-RET in Org.

- If point is in a plain list item: insert a new sibling item at same level.
- If the current item has a checkbox, ensure the new item has an unchecked checkbox [ ].
  Place point right after the checkbox (after \"[ ] \").
- Otherwise (not in a list item): insert a new heading (respecting content).
  If the previous line had a TODO keyword, carry it over."
  (interactive "P")
  (require 'org)
  (if (org-in-item-p)
      (let ((had-checkbox (org-at-item-checkbox-p)))
        ;; Make sure we're on the item's line before inserting
        (end-of-line)
        (org-insert-item)
        ;; Now we're on the new item.
        (when had-checkbox
          (beginning-of-line)
          ;; If no checkbox exists on the new item, insert "[ ] "
          (unless (looking-at-p "^[ \t]*[-+*][ \t]+\\(\\[[ Xx-]\\][ \t]+\\)")
            (re-search-forward "^[ \t]*[-+*][ \t]+" (line-end-position) t)
            (insert "[ ] "))
          ;; Force it to be unchecked (in case Org inserted a checked one)
          (when (re-search-forward "\\[[ Xx-]\\]" (line-end-position) t)
            (replace-match "[ ]" t t))
          ;; Put point after "[ ] "
          (beginning-of-line)
          (re-search-forward "\\[ \\][ \t]*" (line-end-position) t)))
    (let ((prev-todo (org-get-todo-state)))
      (org-insert-heading-respect-content arg)
      (when prev-todo
        (org-todo prev-todo)))))


(setq org-refile-targets
      '(("~/Desktop/org/00-refile.org"      :maxlevel . 1)
	("~/Desktop/org/01-today.org"      :maxlevel . 3)
	("~/Desktop/org/03-projects.org"      :maxlevel . 2)
	("~/Desktop/org/04-areas.org"      :maxlevel . 2)
        ("~/Desktop/org/05-resources.org" :maxlevel . 4)
	("~/Desktop/org/06-archives.org" :maxlevel . 2)
	("~/Desktop/org/07-school.org"      :maxlevel . 3)
	("~/Desktop/org/08-shopping.org"      :maxlevel . 2)))



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

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "g")
    (lambda ()
      (interactive)
      (let ((revert-without-query '(".*")))
        (pdf-view-revert-buffer nil t)))))

(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(setq org-hide-emphasis-markers t)

(global-set-key (kbd "C-z") #'toggle-frame-maximized)

(global-set-key (kbd "C-c l") #'org-store-link)

(add-hook 'org-mode-hook #'visual-line-mode)

;; Keep checklist mode when making new list items.
(setq org-list-automatic-checkbox t)

;; shortcut to minimize emacs frame
(global-set-key (kbd "C-c m") #'iconify-frame)

;; org alerts for macos notifications (date-aware, :alert: only)
(use-package alert
  :ensure t
  :config
  (alert-define-style 'macos-test
    :title "macOS Notification"
    :notifier
    (lambda (info)
      (when (fboundp 'ns-do-applescript)
        (let* ((raw-msg   (or (plist-get info :message) ""))
               (raw-title (or (plist-get info :title) "Emacs"))

               ;; Strip text properties + force plain strings
               (msg   (substring-no-properties (format "%s" raw-msg)))
               (title (substring-no-properties (format "%s" raw-title)))

               ;; Remove newlines (AppleScript can choke on them)
               (msg   (replace-regexp-in-string "[\r\n]+" " " msg))
               (title (replace-regexp-in-string "[\r\n]+" " " title))

               ;; Quote safely for AppleScript
               (msgq   (prin1-to-string msg))
               (titleq (prin1-to-string title))

               ;; Build the AppleScript as a *binding*
               (script (format "display notification %s with title %s sound name %s"
                               msgq titleq (prin1-to-string "Glass"))))
          (condition-case err
              (ns-do-applescript script)
            (error
             (message "AppleScript notify failed: %S" err)
             (message "AppleScript was:\n%s" script))))))

  (setq alert-default-style 'macos-test)))

(use-package org-alert
  :ensure t
  :after org
  :config
  ;; knobs you already use
  (setq org-alert-interval 600
        org-alert-notify-cutoff 10
        org-alert-notify-after-event-cutoff 10)

  ;; -------------------------
  ;; Date-aware :alert: checker
  ;; -------------------------

  (defun danyal/org-alert--collect-timed-timestamps ()
  "Return a list of timestamp strings in the current entry subtree that contain HH:MM."
  (save-excursion
    (org-back-to-heading t)
    (let* ((end (save-excursion (org-end-of-subtree t t)))
           (out '()))
      (while (re-search-forward
              "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^>]*\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)[^>]*>"
              end t)
        (push (match-string 0) out))
      (nreverse out))))

(defun danyal/org-alert--timestamp-next-time (ts now)
  "Given an org timestamp string TS (may include a repeater), return the next occurrence >= NOW as an Emacs time.
If TS has no repeater, return its absolute time."
  (let* ((t0 (org-time-string-to-time ts)))
    ;; If there's no repeater, nothing to expand.
    (if (not (string-match-p "\\+[0-9]+[hdwmy]" ts))
        t0
      ;; Has repeater: advance by the repeater interval until >= now.
      ;; Supports +Nh, +Nd, +Nw, +Nm, +Ny (h/d/w/m/y).
      (let* ((n (string-to-number (replace-regexp-in-string ".*\\+\\([0-9]+\\)\\([hdwmy]\\).*" "\\1" ts)))
             (u (replace-regexp-in-string ".*\\+\\([0-9]+\\)\\([hdwmy]\\).*" "\\2" ts))
             ;; Convert units to seconds (month/year handled approximately via days; good enough for weekly repeaters).
             (step
              (pcase u
                ("h" (* n 3600))
                ("d" (* n 86400))
                ("w" (* n 7 86400))
                ("m" (* n 30 86400))  ;; approximate
                ("y" (* n 365 86400)) ;; approximate
                (_ (* n 7 86400))))
             (cur (float-time t0))
             (nowf (float-time now))
             (guard 0))
        (while (and (< cur nowf) (< guard 5000))
          (setq cur (+ cur step))
          (setq guard (1+ guard)))
        (seconds-to-time cur)))))

(defun danyal/org-alert--entry-time ()
  "Return the next due time (absolute date+time) for this entry, or nil.

Priority:
1) SCHEDULED
2) DEADLINE
3) plain timestamps in subtree

Repeaters (+1w etc.) are expanded to the next occurrence >= now.
If multiple plain timestamps exist, choose the earliest upcoming occurrence."
  (let* ((now (current-time))
         (scheduled (org-entry-get nil "SCHEDULED"))
         (deadline  (org-entry-get nil "DEADLINE")))
    (cond
     (scheduled
      (ignore-errors (danyal/org-alert--timestamp-next-time scheduled now)))
     (deadline
      (ignore-errors (danyal/org-alert--timestamp-next-time deadline now)))
     (t
      (let* ((ts-list (danyal/org-alert--collect-timed-timestamps))
             (times (delq nil
                          (mapcar (lambda (ts)
                                    (ignore-errors (danyal/org-alert--timestamp-next-time ts now)))
                                  ts-list))))
        (when times
          ;; pick earliest upcoming time
          (car (sort times (lambda (a b)
                             (< (float-time a) (float-time b)))))))))))

  (defun danyal/org-alert--check-file (file now cutoff after)
    "Scan FILE for +alert headings and send notifications if within window."
    (with-current-buffer (find-file-noselect file t)
      (org-with-wide-buffer
        (org-map-entries
         (lambda ()
           (let ((tval (danyal/org-alert--entry-time)))
             (when tval
               (let* ((diff-min (/ (float-time (time-subtract tval now)) 60.0))
                      (within (and (<= diff-min cutoff)
                                   (>= diff-min (- after)))))
                 (when within
                   (alert (org-get-heading t t t t)
                          :title (or (bound-and-true-p org-alert-notification-title) "org")
                          :category (or (bound-and-true-p org-alert-notification-category) 'org-alert)
                          :style 'macos-test))))))
         "+alert" 'file))))

  (defun danyal/org-alert-check ()
    "Alert :alert: headlines using absolute date+time comparison (no agenda cache)."
    (interactive)
    (let* ((now (current-time))
           (cutoff (or org-alert-notify-cutoff 10))
           (after  (or org-alert-notify-after-event-cutoff 0))
           (files (org-agenda-files)))
      (dolist (f files)
        (when (and (stringp f) (file-readable-p f))
          (danyal/org-alert--check-file f now cutoff after)))))

  (defvar danyal/org-alert--timer nil
    "Timer for date-aware org alerts.")

  (defun danyal/org-alert-enable ()
    "Enable periodic date-aware :alert: checks."
    (interactive)
    (when (timerp danyal/org-alert--timer)
      (cancel-timer danyal/org-alert--timer))
    (setq danyal/org-alert--timer
          (run-at-time 1 (or org-alert-interval 600) #'danyal/org-alert-check)))

  (defun danyal/org-alert-disable ()
    "Disable periodic date-aware :alert: checks."
    (interactive)
    (when (timerp danyal/org-alert--timer)
      (cancel-timer danyal/org-alert--timer))
    (setq danyal/org-alert--timer nil))

  ;; Stop org-alert's built-in timer if it was enabled elsewhere
  (when (fboundp 'org-alert-disable)
    (ignore-errors (org-alert-disable)))

  ;; Start the date-aware checker
  (danyal/org-alert-enable))

;; Hide lockfiles like .#07-school.org and autosaves like #07-school.org#
(setq counsel-find-file-ignore-regexp
      "\\(?:\\`\\.#\\|\\`#.*#\\'\\)")
