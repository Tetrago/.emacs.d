(setq inhibit-startup-message t ; Disable startup messages
      visible-bell t ; Disable obnoxious beeping
      custom-file "~/.emacs.d/custom.el" ; Keep emacs from editing this file automatically
      make-backup-files nil ; Disable backup clutter
      create-lockfiles nil ; Disable lockfile clutter
      gc-cons-threshold 100000000 ; Increase garbage collection limit
      read-process-output-max (* 1024 1024)) ; Increase process read limit for lsp
(load custom-file)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Map escape to escape
(setq-default truncate-lines -1 ; Disable line trunction
              tab-width 4
              indent-tabs-mode t)

(menu-bar-mode -1) ; Disable menu bar
(tool-bar-mode -1) ; Disable tool bar
(scroll-bar-mode -1) ; Disable scroll bar
(set-fringe-mode 10) ; Extra fringe space
(electric-pair-mode 1) ; Delimiter matching
(auto-save-mode -1) ; Disable auto saving

(column-number-mode) ; Show column numbers
(global-display-line-numbers-mode 1) ; Show line numbers
(setq display-line-numbers-type 'relative) ; Display relative line numbers

(dolist (mode '(org-mode-hook
        term-mode-hook
        eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1)))) ; Disable line numbers for some modes

(set-face-attribute 'default nil :font "FiraCode NF" :height 100)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

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
  (evil-set-undo-system 'undo-redo)
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
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-set-init-info t)
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (setq dashboard-items '((recents . 5) (projects . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                        '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                          ;; =:= =!=
                          ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                          ;; ;; ;;;
                          (";" (rx (+ ";")))
                          ;; && &&&
                          ("&" (rx (+ "&")))
                          ;; !! !!! !. !: !!. != !== !~
                          ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                          ;; ?? ??? ?:  ?=  ?.
                          ("?" (rx (or ":" "=" "\." (+ "?"))))
                          ;; %% %%%
                          ("%" (rx (+ "%")))
                          ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                          ;; |->>-||-<<-| |- |== ||=||
                          ;; |==>>==<<==<=>==//==/=!==:===>
                          ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                          "-" "=" ))))
                          ;; \\ \\\ \/
                          ("\\" (rx (or "/" (+ "\\"))))
                          ;; ++ +++ ++++ +>
                          ("+" (rx (or ">" (+ "+"))))
                          ;; :: ::: :::: :> :< := :// ::=
                          (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                          ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                          ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                          "="))))
                          ;; .. ... .... .= .- .? ..= ..<
                          ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                          ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                          ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                          ;; *> */ *)  ** *** ****
                          ("*" (rx (or ">" "/" ")" (+ "*"))))
                          ;; www wwww
                          ("w" (rx (+ "w")))
                          ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                          ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                          ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                          ;; << <<< <<<<
                          ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                          "-"  "/" "|" "="))))
                          ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                          ;; >> >>> >>>>
                          (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                          ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                          ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                       (+ "#"))))
                          ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                          ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                          ;; __ ___ ____ _|_ __|____|_
                          ("_" (rx (+ (or "_" "|"))))
                          ;; Fira code: 0xFF 0x12
                          ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                          ;; Fira code:
                          "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                          ;; The few not covered by the regexps.
                          "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

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

(use-package popwin
  :config
  (push '(compilation-mode :tail t :height 20) popwin:special-display-config)
  (popwin-mode 1))

(use-package lsp-mode
  :commands lsp
  :init
  (use-package flycheck)
  (add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-snippet nil)
  :config
  (lsp-enable-which-key-integration t)
  :hook (
     (c-mode . lsp)
     (c++-mode . lsp)
     (cmake-mode . lsp)
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
  (setq projectile-enable-caching nil)
  (setq projectile-completion-system 'ivy)
  (projectile-mode 1))

(use-package counsel-projectile
  :after (counsel projectile))

(use-package magit
  :commands magit-status)

(defun local/org-mode ()
  (org-indent-mode 1)
  (org-modern-mode 1)
  (visual-line-mode 1)
  (local/visual-fill))

(use-package org
  :commands org-mode
  :pin org
  :hook (org-mode . local/org-mode)
  :config
  (require 'org-mouse)
  (use-package org-modern))

(defun local/visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :commands visual-fill-column-mode)

(use-package good-scroll
  :config
  (good-scroll-mode 1))

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

(use-package devdocs)

(use-package expand-region)

(use-package rust-mode
  :commands rust-mode)

(use-package cmake-mode
  :config
  (setq cmake-tab-width 4)
  :commands cmake-mode)

(setq c-default-style "bsd")
(setq-default c-basic-offset 4)

(use-package markdown-mode)

(use-package json-mode)

(general-define-key
  :states 'normal
  "/" 'counsel-grep-or-swiper ; Search
  "C-k" 'counsel-projectile-find-file ; Fuzzy file finder
  "C-i" 'lsp-ui-doc-glance ; Lsp parameters
  "C-b" 'projectile-compile-project ; Compile
  "C-a" 'er/expand-region ; Expand region
  "<f5>" 'dap-continue ; Continue form breakpoint
  "<f9>" 'dap-breakpoint-toggle ; Toggle breakpoint
  "<f10>" 'dap-next ; Step over
  "<f11>" 'dap-step-in ; Step in
  "<f12>" 'dap-step-out) ; Step out

(local/leader-key
  "f" '(counsel-find-file :which-key "browse files")
  "q" '(flycheck-list-errors :which-key "lsp quickfix")
  "p" '(counsel-projectile-switch-project :which-key "switch project")
  "a" '(:ignore t :which-key "actions")
  "a c" '(projectile-configure-project :which-key "configure project")
  "a b" '(projectile-compile-project :which-key "compile project")
  "a r" '(projectile-run-project :which-key "run project")
  "a t" '(projectile-test-project :which-key "test project")
  "g" '(magit-status :which-key "git")
  "t" '(treemacs :which-key "tree")
  "d" '(:ignore t :which-key "debug")
  "d l" '(dap-debug :which-key "launch")
  "d e" '(dap-debug-edit-template :which-key "edit")
  "d q" '(dap-delete-session :which-key "quit")
  "D" '(:ignore t :which-key "docs")
  "D l" '(devdocs-lookup :which-key "lookup")
  "D i" '(devdocs-install :which-key "install")
  "D d" '(devdocs-delete :which-key "delete")
  "D u" '(devdocs-update-all :which-key "update"))
