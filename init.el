(setq inhibit-startup-message t) ; Disable startup messages
(setq visible-bell t) ; Disable obnoxious beeping
(setq custom-file :noerror) ; Keep emacs from editing this file automatically
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Map escape to escape

;; Modes
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

(column-number-mode) ; Show column numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative) ; Relative line numbers

;; Paths
(add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))

;; Hooks
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1)))) ; Disable line numbers for some modes

;; Font
(set-face-attribute 'default nil :font "FiraCode NF" :height 100)

;; Setup packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Packages
(use-package ivy
  :config
  (ivy-mode 1)
  (use-package ivy-rich
    :init
    (ivy-rich-mode 1)))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  :requires ivy)

(use-package swiper
  :requires ivy)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-g-bindings t)
  :config
  (evil-mode 1)
  (use-package evil-commentary)
  (use-package evil-collection
    :config
    (evil-collection-init)))

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

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
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
  :init
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-headerline-breadcrumb-enable nil)
  :hook (
	 (c++-mode . lsp)
	 (rust-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :requires lsp-mode)

(use-package company
  :config
  (global-company-mode 1)
  :requires lsp-mode)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1)
  :requires lsp-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  :requires (lsp-mode ivy))

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1))

(use-package counsel-projectile
  :requires (counsel projectile))

(use-package dashboard
  :config
  (setq dashboard-banner-logo-title "Welcome to Turbo")
  (setq dashboard-startup-banner 3)
  (setq dashboard-center-content t)
  (setq dashboard-set-init-info t)
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (dashboard-setup-startup-hook))

(use-package magit)

;; Key bindings
(general-define-key
 "C-k" 'counsel-projectile-find-file ; Fuzzy file finder
 "<f5>" 'projectile-run-project)

(general-define-key
 :states 'normal
 "/" 'counsel-grep-or-swiper) ; Search

(local/leader-key
  "f" '(counsel-dired :which-key "browse files")
  "q" '(lsp-treemacs-quick-fix :which-key "lsp quickfix")
  "p" '(counsel-projectile-switch-project :which-key "open")
  "g" '(magit-status :which-key "magit"))
