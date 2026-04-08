;; ~/.config/emacs/init.el

;; rustup component add rust-analyzer.

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Initialize package system
(package-refresh-contents) ;; Only needed the first time to fetch package lists
(package-install 'use-package)

;; Enable use-package
(eval-when-compile
  (require 'use-package))

;; Configure Rustic mode (recommended over rust-mode)
(use-package rustic
  :ensure t
  :config
  (defun my/rustic-mode-hook ()
    (setq indent-tabs-mode nil) ;; Use spaces for indentation
    (lsp-deferred)             ;; Start LSP asynchronously
    (if (featurep 'yasnippet)  ;; Optional: enable snippets if you use yasnippet
        (yas-minor-mode)))
  :hook
  (rustic-mode . my/rustic-mode-hook))

;; Configure LSP mode (provides language server support)
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; Set a prefix for LSP keybindings
  :config
  (lsp-mode 1))

;; Optional: Add packages for better LSP experience
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package flycheck
  :ensure t
  :after lsp-mode
  :config
  (add-hook 'lsp-mode-hook 'flycheck-mode)) ;; Use flycheck with lsp-mode

;; Configure Cargo commands (provides build/run/test shortcuts)
(use-package cargo
  :ensure t
  :hook
  (rustic-mode . cargo-minor-mode)) ;; Automatically enable cargo minor mode in rustic mode

;; Magit: The best Git porcelain for Emacs
(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

;; Projectile: Project management
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  :bind
  ("C-c p" . projectile-command-map))

;; Yasnippet: Code snippets
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

===========================================================================================





(setq package-enable-at-startup nil
      inhibit-startup-message   t
      frame-resize-pixelwise    t  ; fine resize
      package-native-compile    t) ; native compile packages
(scroll-bar-mode -1)               ; disable scrollbar
(tool-bar-mode -1)                 ; disable toolbar
(tooltip-mode -1)                  ; disable tooltips
(set-fringe-mode 10)               ; give some breathing room
(menu-bar-mode -1)                 ; disable menubar
(blink-cursor-mode 0)              ; disable blinking cursor










================================================================================================
;; XDG-compatible ~/.config/emacs/init.el.
;; Configure package sources
(setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; Bootstrap use-package: Install it if it's not already present
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package-always-ensure to automatically install packages
(setq use-package-always-ensure t)

;; Basic rust-mode configuration
(use-package rust-mode
  :hook (rust-mode . lsp) ;; Or (rust-mode . eglot-ensure) if you prefer eglot
  :bind (:map rust-mode-map
              ("C-c C-t" . racer-describe) ;; Example keybinding
              ("C-c C-c C-r" . rust-run)   ;; Run the current Rust program
              ("C-c C-c C-b" . rust-compile) ;; Compile the current Rust program
              ("C-c C-c C-t" . rust-test))  ;; Test the current Rust project
  :config
  ;; Additional configuration can go here. For example, enabling tree-sitter:
  (setq rust-mode-treesitter-derive t))
  ;; Configuration for TOML files (Cargo.toml)
(use-package toml-mode)

;; Configuration for Cargo integration
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;; Example for using Eglot (requires the rust-analyzer LSP server installed on your system)
(use-package eglot
  :hook ((rust-mode . eglot-ensure)
         (before-save . eglot-format-buffer))
  :config
  ;; Ensure rust-analyzer uses the correct project root
  (add-to-list 'eglot-server-programs '((rust-mode) "rust-analyzer"))
  ;; Other Eglot configurations...
  )


  ==========================================
  (use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save t))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook (rust-mode . eglot-ensure)
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save t))
========================================
(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :init
  ;; Remap the standard rust-mode to rust-ts-mode
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
  :config
  (setq rust-format-on-save t))


(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :hook (rust-ts-mode . eglot-ensure)
  :init
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode)))

(use-package cargo
  :ensure t
  :hook (rust-ts-mode . cargo-minor-mode))
;; Font Locking: Tree-sitter supports different "levels" of highlighting. You can adjust this with (setq treesit-font-lock-level 4)

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :init
  ;; Remap standard rust-mode to rust-ts-mode
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
  :hook (rust-ts-mode . eglot-ensure)
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :hook (rust-ts-mode . cargo-minor-mode))
====================================================================
;; 1. Automatic Grammar Management
;; rust-ts-mode requires a compiled grammar. This package downloads it for you.
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; 2. The Core Rust Tree-Sitter Mode
(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :init
  ;; Remap standard rust-mode to the Tree-Sitter version
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
  :hook (rust-ts-mode . eglot-ensure) ;; Start LSP automatically
  :config
  ;; Maximize Tree-sitter highlighting detail
  (setq treesit-font-lock-level 4)
  ;; Optional: auto-format on save if you have rustfmt installed
  (setq rust-format-on-save t))

;; 3. Autocompletion & LSP (Built-in Eglot)
(use-package eglot
  :config
  ;; Enable inlay hints (type annotations shown in-line)
  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)
  ;; Use Clippy for real-time linting instead of standard check
  (setq-default eglot-workspace-configuration
                '(:rust-analyzer (:check (:command "clippy")))))

;; 4. Cargo Integration
(use-package cargo
  :ensure t
  :hook (rust-ts-mode . cargo-minor-mode))

;; 5. Completion UI (Corfu)
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2))

;; rustup component add rust-analyzer rustfmt clippy
===============================================================
(load-theme 'theme-name t)
===============================================

(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord t))
===============================================================
(load-theme 'nord t)
=================================================================
;; Disable the menu bar, scroll bar, and tool bar for a minimal interface
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Hide the startup screen
(setq inhibit-startup-screen t)

;; Enable the package system (necessary for installing community packages)
;; In newer Emacs versions, this might be enabled by default, but this ensures it.
;; (package-initialize) 
===================================================================
(setopt treesit-font-lock-level 4)
===================================================================
(add-hook 'rust-mode-hook 'cargo-minor-mode)
=====================================================================
;; Emacs 29+ syntax
(keymap-global-set "C-t" #'whitespace-mode)
(keymap-set texinfo-mode-map "C-c C-c g" 'texinfo-insert-@group)
=======================================================================
M-x customize-variable RET tool-bar-mode
Turn off toolbar
M-x customize-variable RET menu-bar-mode
Turn off menubar
M-x customize-variable RET scroll-bar-mode
Turn off scrollbar.
M-x customize-variable RET blink-cursor-mode
Also see M-x customize-group RET Cursor RET
====================================================================
;; --- User Interface Customizations ---

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar (top icons)
(tool-bar-mode -1)

;; Disable the vertical scroll bar
(scroll-bar-mode -1)

;; Disable the horizontal scroll bar
(horizontal-scroll-bar-mode -1)

;; --- Additional Modern UI Enhancements ---

;; Enable smooth scrolling (requires Emacs 29+)
(pixel-scroll-precision-mode 1)

;; Disable startup splash screen
(setq inhibit-startup-screen t)

;; Hide tab bar (if used)
(tab-bar-mode -1)
====================================================
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
===========================================

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("C-c C-c C-r" . rustic-cargo-run)
              ("C-c C-c C-t" . rustic-cargo-test))
  :config
  (setq rustic-format-on-save t) ; Auto-format with rustfmt
  (setq rustic-lsp-client 'eglot)) ; Use eglot for LSP

(use-package eglot
  :ensure nil ; Built-in in Emacs 29+
  :hook (rustic-mode . eglot-ensure))
=====================================================
;; rust-ts-mode is the built-in (since Emacs 29) major mode that uses Tree-sitter for advanced syntax highlighting and indentation.
;; You can use them together by configuring rustic-mode to use rust-ts-mode as its foundation rather than the older rust-mode.
(use-package lsp-mode
  :ensure t
  :hook ((rustic-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-rust-analyzer-cargo-watch-command "clippy") ; Real-time linting
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-peek-enable t
        lsp-ui-sideline-show-hover t))

(use-package consult-lsp
  :ensure t
  :bind ("M-s l" . consult-lsp-symbols))
============================================================
(set-frame-font "Desired Font Name-Size" nil t)
(set-face-attribute 'default nil :family "Monospace" :height 120)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)

=========================================================
;; Enable the use-package macro for easier configuration
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org") t)
(package-refresh-contents) ;; You may want to run this manually first

(unless (package-installed-p 'cargo)
  (package-install 'cargo))
(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))
(unless (package-installed-p 'toml-mode)
    (package-install 'toml-mode)) ;; Useful for editing Cargo.toml files

;; Configure rust-mode to use rust-ts-mode (tree-sitter) if available
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  ;; Enable rust-ts-mode if using Emacs 29.1+ with tree-sitter support
  ;; The variable below is a way to opt-into using tree-sitter in rust-mode
  (setq rust-mode-treesitter-derive t)
  :hook
  ;; Add cargo-minor-mode to rust-mode hooks
  (rust-mode-hook . cargo-minor-mode)
  (rust-mode-hook . (lambda () (display-line-numbers-mode 1))))

;; Add cargo-minor-mode to toml-mode hooks for managing dependencies
(add-hook 'toml-mode-hook 'cargo-minor-mode)

;; Optional: Add a custom function to run cargo and handle user input
;; This is useful if you have a Rust program that reads from stdin
(defun my-cargo-run-interactive ()
  "Build and run Rust code, allowing user input."
  (interactive)
  (cargo-process-run)
  (let* ((orig-win (selected-window))
         (run-win (display-buffer (get-buffer "*Cargo Run*") nil 'visible)))
    (select-window run-win)
    (comint-mode)
    (read-only-mode 0)
    (select-window orig-win)))

;; Bind the custom interactive run function to C-c C-c C-r in rust-mode
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-c C-c C-r") 'my-cargo-run-interactive))

;; Ensure cargo is in the Emacs executable path if needed
;; (exec-path-from-shell-initialize) ;; uncomment if Emacs struggles to find cargo
=================================================================
(defun my/rust-ts-mode-hook ()
  "Custom functions to run when `rust-ts-mode` is enabled."
  (setq indent-tabs-mode nil) ;; Use spaces instead of tabs for indentation
  (lsp-deferred)) ;; Start LSP server if not already running
  
(add-hook 'rust-ts-mode-hook 'my/rust-ts-mode-hook)
==========================
(use-package rust-mode
  :ensure t
  :init
  ;; Enable treesitter derive feature if needed by your config
  (setq rust-mode-treesitter-derive t) 
  :config
  (defun my/rust-mode-config ()
    "Configuration for rust-mode and rust-ts-mode."
    (setq indent-tabs-mode nil)
    (lsp-deferred))
  :hook
  ;; Add the custom function to both rust-mode-hook and rust-ts-mode-hook
  (rust-mode . my/rust-mode-config)
  (rust-ts-mode . my/rust-mode-config))
===========================================================================
;; Daemon/Client Issues: If you use emacsclient, you may need to wrap these in a hook to ensure they apply to new frames:
;; Disable vs. Toggle: Using -1 forces the mode off. Using 0 also works to turn it off, while 1 turns it on, and nil toggles it.
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (tool-bar-mode -1)
              (menu-bar-mode -1)
              (scroll-bar-mode -1))))
=========================================================
(use-package dap-mode
  :ensure t
  :config
  (dap-mode t))

(use-package dap-lldb
  :ensure t
  :after dap-mode
  :config
  ;; Configuration might be needed to point to the correct lldb-vscode executable
  (dap-lldb-setup))

(use-package rustic
  :ensure t
  :config
  (add-hook 'rustic-mode-hook (lambda ()
                                (dap-register-debug-provider "lldb" 'dap-lldb-rust--populate-start-file-args))))
======================================================
(column-number-mode)
(display-line-numbers-mmode)
(setq inhibit-startup-message t)
(setq rustic-format-on-save t)
==============================================
(use-package clippy-flymake)?????????????
(use-package cargo-mode)
(use-package exec-path-from-shell)
(use-package flycheck-rust)
(use-package cargo)
(use-package racer)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(use-package ivy)
(use-package lsp-mode)
(use-package lsp-ui)
(use-package projectile)
(use-package magit)
(use-package ace-window)
(require 'org)
(use-package dap-mode)
(use-package smartparens)
(use-package company-box)
(use-package forge)
