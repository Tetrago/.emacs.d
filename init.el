(setq inhibit-startup-message t) ; Disable startup messages
(setq visible-bell t) ; Disable obnoxious beeping
(setq custom-file :noerror) ; Keep emacs from editing this file automatically
(setq mouse-wheel-progressive-speed nil) ; Disable scroll acceleration

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Map escape to escape
(setq-default tab-width 4) ; Tab width to 4 instead of 8
(setq-default truncate-lines -1) ; Disable line truncation

(menu-bar-mode -1) ; Disable menu bar
(tool-bar-mode -1) ; Disable tool bar
(scroll-bar-mode -1) ; Disable scroll bar
(set-fringe-mode 10) ; Extra fringe space
(electric-pair-mode 1) ; Delimiter matching

(column-number-mode) ; Show column numbers
(global-display-line-numbers-mode 1) ; Show line numbers
(setq display-line-numbers-type 'relative) ; Display relative line numbers

(dolist (mode '(org-mode-hook
        term-mode-hook
        eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1)))) ; Disable line numbers for some modes

(defun local/org-mode ()
  (org-indent-mode 1)
  (org-modern-mode 1)
  (visual-line-mode 1)
  (local/visual-fill))

(defun local/visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(set-face-attribute 'default nil :font "FiraCode NF" :height 100)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
    (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

(use-package ivy
  :config
  (ivy-mode 1)
  (use-package ivy-rich
    :init
    (ivy-rich-mode 1)))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  :config
  (setq counsel-find-file-ignore-regexp "#.+#")
  :after ivy)

(use-package swiper
  :after ivy)

(use-package evil
  :init
  (setq evil-want-integration t) ; Extra integration
  (setq evil-want-keybinding nil) ; Don't add unnecessary key bindings
  (setq evil-want-C-u-scroll t) ; Enable C-u scroll
  (setq evil-want-C-d-scroll t) ; Enable C-d scroll
  (setq evil-want-C-i-jump nil) ; Emacs key binding fix
  (setq evil-want-C-g-bindings t) ; Enable C-g to quit
  :config
  (evil-mode 1)
  ;; Visual line mode motion fix
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (use-package evil-collection
    :config
    (evil-collection-init))
  (use-package evil-surround
    :config
    (global-evil-surround-mode 1)))

(use-package all-the-icons)

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  :after (all-the-icons))

(use-package dashboard
  :config
  (setq dashboard-banner-logo-title "Welcome to Turbo")
  (setq dashboard-startup-banner 3)
  (setq dashboard-center-content t)
  (setq dashboard-set-init-info t)
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (dashboard-setup-startup-hook))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer local/leader-key
    :keymaps '(normal emacs)
    :prefix "SPC"))

(use-package lsp-mode
  :commands lsp
  :init
  (use-package flycheck)
  (add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-headerline-breadcrumb-enable nil)
  :config
  (lsp-enable-which-key-integration t)
  :hook (
     (c-mode . lsp)
     (c++-mode . lsp)
     (rust-mode . lsp)
     (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :after lsp-mode)

(use-package company
  :config
  (global-company-mode 1)
  (use-package company-box
    :hook company-mode)
  :after lsp-mode)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1)
  :after (lsp-mode treemacs))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  :after (lsp-mode ivy))

(use-package dap-mode
  :commands dap-debug
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (require 'dap-cpptools))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode 1))

(use-package counsel-projectile
  :after (counsel projectile))

(use-package magit
  :commands magit-status)

(use-package org
  :commands org-mode
  :pin org
  :hook (org-mode . local/org-mode)
  :config
  (require 'org-mouse)
  (use-package org-modern))

(use-package visual-fill-column
  :commands visual-fill-column-mode)

(use-package treemacs
  :commands treemacs)

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package rust-mode
  :commands rust-mode)

(general-define-key
 "C-k" 'counsel-projectile-find-file ; Fuzzy file finder
 "C-i" 'lsp-ui-doc-glance ; Lsp parameters
 "C-b" 'compile ; Compile
 "<f5>" 'projectile-run-project) ; Run project

(general-define-key
 :states 'normal
 "/" 'counsel-grep-or-swiper) ; Search

(local/leader-key
  "f" '(counsel-find-file :which-key "browse files")
  "q" '(flycheck-list-errors :which-key "lsp quickfix")
  "p" '(counsel-projectile-switch-project :which-key "open")
  "g" '(magit-status :which-key "git")
  "t" '(treemacs :which-key "tree")
  "d" '(:ignore t :which-key "debug")
  "d l" '(dap-debug :which-key "launch")
  "d e" '(dap-debug-edit-template :which-key "edit"))
