

----------------------------------
(use-package package
  :ensure nil
  :config
  (package-initialize)
  :custom
  (package-native-compile t)
  (package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/"))))
--------------------------------------
;; started from http://emacs-bootstrap.com/

;; rust-mode
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :bind ( :map rust-mode-map
               (("C-c C-t" . racer-describe)
                ([?\t] .  company-indent-or-complete-common)))
  :config
  (progn
    ;; add flycheck support for rust (reads in cargo stuff)
    ;; https://github.com/flycheck/flycheck-rust
    (use-package flycheck-rust)

    ;; cargo-mode for all the cargo related operations
    ;; https://github.com/kwrooijen/cargo.el
    (use-package cargo
      :hook (rust-mode . cargo-minor-mode)
      :bind
      ("C-c C-c C-n" . cargo-process-new)) ;; global binding

    ;;; separedit ;; via https://github.com/twlz0ne/separedit.el
    (use-package separedit
      :straight (separedit :type git :host github :repo "idcrook/separedit.el")
      :config
      (progn
        (define-key prog-mode-map (kbd "C-c '") #'separedit)
        (setq separedit-default-mode 'markdown-mode)))


    ;;; racer-mode for getting IDE like features for rust-mode
    ;; https://github.com/racer-rust/emacs-racer
    (use-package racer
      :hook (rust-mode . racer-mode)
      :config
      (progn
        ;; package does this by default ;; set racer rust source path environment variable
        ;; (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
        (defun my-racer-mode-hook ()
          (set (make-local-variable 'company-backends)
               '((company-capf company-files)))
          (setq company-minimum-prefix-length 1)
          (setq indent-tabs-mode nil))

        (add-hook 'racer-mode-hook 'my-racer-mode-hook)

        ;; enable company and eldoc minor modes in rust-mode (racer-mode)
        (add-hook 'racer-mode-hook #'company-mode)
        (add-hook 'racer-mode-hook #'eldoc-mode)))

    (add-hook 'rust-mode-hook 'flycheck-mode)
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

    ;; format rust buffers on save using rustfmt
    (add-hook 'before-save-hook
              (lambda ()
                (when (eq major-mode 'rust-mode)
                  (rust-format-buffer))))))
-------------------------------------------------------
;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;; from https://github.com/rakanalh/emacs-bootstrap
;;    / http://emacs-bootstrap.com

;;; Code:

;;______________________________________________________________________
;;;;  package management related


;;____________________________________________________________________________
;;;;  straight.el
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#getting-started
;; customizing check variable to speedup emacs startup
(setq straight-check-for-modifications '(check-on-save find-when-checking))
;;(setq straight-check-for-modifications 'nil)

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


;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;;; https://github.com/raxod502/straight.el#updating-recipe-repositories
;;
;; updating a recipe repository (e.g. melpa) to get a newish PACKAGE-NAME
;;
;;     M-x straight-pull-package   melpa
;;     M-x straight-use-package    PACKAGE-NAME

;; set load path
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
;; ;; ad-handle-definition: ‘align-regexp’ got redefined
;; ;; ad-handle-definition: ‘find-tag-regexp’ got redefined
(setq ad-redefinition-action 'accept)

;; https://emacs.stackexchange.com/questions/74289/emacs-28-2-error-in-macos-ventura-image-type-invalid-image-type-svg#comment127558_74801
;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=59081
(if (version< emacs-version "29.0")
    ;; alias to other function
    (add-to-list 'image-types 'svg)
  ;; should be fixed in emacs 29
  )



(require 'base)                ;; basic setup
(require 'base-theme)          ;; pick themes
(require 'base-functions)      ;; utility functions
(require 'base-platforms)      ;; handle macOS, Linux, etc.
(require 'base-extensions)     ;; bulk of config not found below
(require 'base-global-keys)    ;; add bindings for functions

(require 'base-lsp)            ;; Language Server Protocol
(require 'lang-shell)          ;; shell-related (zsh, eshell, bash, etc)
(require 'lang-python)
(require 'lang-perl)
(require 'lang-ruby)
(require 'lang-go)
(require 'lang-elixir)
(require 'lang-apple)
(require 'lang-cpp)
(require 'lang-rust)
(require 'lang-javascript)
(require 'lang-lisp)
(require 'lang-web)
(require 'lang-hardware)
(require 'lang-other)

(require 'base-finally)  ;; any final cleanup

(provide 'init)

;;; init.el ends here
--------------------------------
;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

;; ;; the following is for previous versions of emacs; the early-init.el file
;; ;; is not even loaded in versions before 27 however, and package-initialize
;; ;; would be in init.el; keeping here (commented out) for historical reasons
;; (when (< emacs-major-version 27)
;;   (package-initialize))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;; ----------------------------------------------------------------------------
;; Make startup faster by reducing the frequency of garbage collection, and
;; restore smaller value after startup.  Emacs default is 800,000 bytes.
;; Measured in bytes.
(defvar normal-gc-cons-threshold (* 800 1024)
  "The post-init value to use for `gc-cons-threshold'.
If you experience freezing, decrease this. If you experience stuttering, increase this.")

(let ((init-gc-cons-threshold (* 500 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            #'(lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; https://blog.d46.us/advanced-emacs-startup/
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)))

;; ;; use as debug aid: backtrace to show what is loading org
;; (eval-after-load "org" '(debug))
;; emacs28's included org has bug so can use to find early load triggers that
;;; pull in built-in org
;;; - magit-todos
;;; - org-bullets

;; ;; use as debug aid: backtrace to show what is loading recentf
;; (eval-after-load "recentf" '(debug))
;;; loads recentf:
;;; - dashboard

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
---------------------------------------------------------
(require 'package)

(defmacro append-to-list (target suffix)
  "Append SUFFIX to TARGET in place."
  `(setq ,target (append ,target ,suffix)))

(append-to-list package-archives
                '(("melpa" . "http://melpa.org/packages/") ;; Main package archive
                  ("melpa-stable" . "http://stable.melpa.org/packages/") ;; Some packages might only do stable releases?
                  ("org-elpa" . "https://orgmode.org/elpa/"))) ;; Org packages, I don't use org but seems like a harmless default

(package-initialize)

;; Ensure use-package is present. From here on out, all packages are loaded
;; with use-package. Also, refresh the package archive on load so we can pull the latest packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Allow navigation between use-package stanzas with imenu.
;; This has to be set before loading use-package.
(defvar use-package-enable-imenu-support t)
(require 'use-package)
(setq
 use-package-always-ensure t ;; Makes sure to download new packages if they aren't already downloaded
 use-package-verbose t) ;; Package install logging. Packages break, nice to know why.

;; Any Customize-based settings should live in custom.el, not here.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(use-package doom-themes
  :init
  (load-theme 'doom-one))

;;; OS specific config
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

;; Emacs feels like it's developed with linux in mind, here are some mac UX improvments
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq default-input-method "MacOSX"))

;; Some linux love, too
(when *is-a-linux*
  (setq x-super-keysym 'meta))

;; Fullscreen by default, as early as possible. This tiny window is not enough
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Make M-x and other mini-buffers sortable, filterable
(use-package ivy
  :init
  (ivy-mode 1)
  (unbind-key "S-SPC" ivy-minibuffer-map)
  (setq ivy-height 15
        ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t))

(use-package counsel
  :after ivy
  :init
  (counsel-mode 1)
  :bind (:map ivy-minibuffer-map))

;; Company is the best Emacs completion system.
(use-package company
  :bind (("C-." . company-complete))
  :custom
  (company-idle-delay 0) ;; I always want completion, give it to me asap
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 10 "The more the merrier.")
  :config
  (global-company-mode) ;; We want completion everywhere

  ;; use numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))

;; Flycheck is the newer version of flymake and is needed to make lsp-mode not freak out.
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Package for interacting with language servers
(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil ;; Flymake is outdated
        lsp-headerline-breadcrumb-mode nil)) ;; I don't like the symbols on the header a-la-vscode, remove this if you like them.

;; Basic python configuration
(use-package python
  :init
  (require 'python)
  :config
  ;; As soon as we detect python go look for that virtual env.
  (add-hook 'python-mode-hook #'auto-virtualenv-set-virtualenv))

;; Pyenv is the only reasonable way to manage python installs. What a mess.
(use-package pyvenv)

;; Auto detect my virtual env, please
(use-package auto-virtualenv)

;; Use the microsoft-python language server, it's the best available as of writing this
(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

;; We need something to manage the various projects we work on
;; and for common functionality like project-wide searching, fuzzy file finding etc.
(use-package projectile
  :init
  (projectile-mode t) ;; Enable this immediately
  :config
  (setq projectile-enable-caching t ;; Much better performance on large projects
        projectile-completion-system 'ivy)) ;; Ideally the minibuffer should aways look similar

;; Counsel and projectile should work together.
(use-package counsel-projectile
  :init
  (counsel-projectile-mode))

;; This is the best git client ever.
;; I know some people who only use emacs for magit (i.e dev in IntelliJ and commit/rebase/diff in emacs)
;; it's that good.
(use-package magit
  :config
  (magit-auto-revert-mode t) ;; Buffers will reload automatically if for example you hard-reset/revert changes in magit
  (setq magit-completing-read-function 'ivy-completing-read)) ;; Everything should work with Ivy


;; Begin improvements

;; The best package for managing opened files that I've ever used.
;; No more tabbing through 10 open files, and no more counting your open files to Cmd-num to.
(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :config
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "deep sky blue"
                      :weight 'bold
                      :height 2.0) ;; Some nice formatting to make the letters more visible
  (setq aw-scope 'frame)
  (setq aw-dispatch-always t)
  (setq aw-keys '(?q ?w ?e ?r ?a ?s ?d ?f))
  (setq aw-dispatch-alist '((?c aw-swap-window "Ace - Swap Window")
                            (?n aw-flip-window)))
  (ace-window-display-mode t))

;; Sort M-x by recently used, I can't believe this isn't a default already.
(use-package smex)

;; Begin evil config

;; Optional for vim users
;; (use-package evil
;;   :init
;;   (evil-mode t))
------------------------------------------------------------------------------------------------------

;; (eval-and-compile
;;   (require 'package)
;;   (setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
;;                            ("marmalade" . "https://marmalade-repo.org/packages/")
;;                            ("melpa" . "https://melpa.org/packages/")))
;;   (package-initialize)
;;   ;; i always fetch the archive contents on startup and during compilation, which is slow
;;   (package-refresh-contents)
;;   (unless (package-installed-p 'use-package)
;;     (package-install 'use-package))
;;   (require 'use-package)
;;   ;; i don't really know why this isn't the default...
;;   (setf use-package-always-ensure t))
;;create init.org --> create emacs-init.el
;;(require 'org)
;;(org-babel-load-file (expand-file-name "init.org" user-emacs-directory ;replace user-emacs-directory with actual path to your init.org
;;ensure (package-initialize) is called early-before any org-related functions or code blocks in your init.org
;;straight.el > add this "bootstrapping"script
;; (straight-use-package 'evil)
;;disable splash screen and startup message
;;(setq inhibit-startup-message t) 
;; (setq initial-scratch-message nil)
;;package-vc-install function to install packages from github
;; (require 'package)
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-archives
;;    (quote
;;     (("gnu" . "https://elpa.gnu.org/packages/")
;;      ("melpa" . "https://melpa.org/packages/")))))
;; (package-initialize)
;; To use Melpa:
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/") t)
;; To use Melpa-Stable:
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; https://jwiegley.github.io/use-package/installation/
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
;; (require 'use-package)

;; (with-eval-after-load 'info
;;   (info-initialize)
;;   (add-to-list 'Info-directory-list
;;                "~/.emacs.d/site-lisp/use-package/"))
---------------------------------
;; Note that I use use-package for Emacs package management. 
;; It will be auto-installed in the standalone version of this config. 
;; Otherwise you can add a snippet like below to your init.el:

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))



(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))




(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))




(use-package company
  ;; ... see above ...
  (:map company-mode-map
	("<tab>". tab-indent-or-complete)
	("TAB". tab-indent-or-complete)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))





(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(use-package dap-mode
  :ensure
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	 :gdbpath "rust-lldb"
         :target nil
         :cwd nil)))





----------------------------------
(use-package smartparens :ensure t
  :config (require 'smartparens-rust))

(defun sp1ff/rust/mode-hook ()
  "My rust-mode hook"

  (column-number-mode)
  (display-line-numbers-mode)
  (hs-minor-mode)
  (smartparens-mode)
  (define-key rust-mode-map "\C-ca" 'eglot-code-actions)
  (define-key rust-mode-map (kbd "C-<right>")   'sp-forward-slurp-sexp)
  (define-key rust-mode-map (kbd "C-<left>")    'sp-forward-barf-sexp)
  (define-key rust-mode-map (kbd "C-M-<right>") 'sp-backward-slurp-sexp)
  (define-key rust-mode-map (kbd "C-M-<left>")  'sp-backward-barf-sexp)
  (define-key rust-mode-map "\C-c>" 'hs-show-all)
  (define-key rust-mode-map "\C-c<" 'hs-hide-all)
  (define-key rust-mode-map "\C-c;" 'hs-toggle-hiding)
  (define-key rust-mode-map "\C-c'" 'hs-hide-level)
  (setq indent-tabs-mode nil
        tab-width 4
        c-basic-offset 4
        fill-column 100))

(use-package rust-mode
  :ensure t
  :hook (rust-mode . sp1ff/rust/mode-hook)
  :config
  (let ((dot-cargo-bin (expand-file-name "~/.cargo/bin/")))
    (setq rust-rustfmt-bin (concat dot-cargo-bin "rustfmt")
          rust-cargo-bin (concat dot-cargo-bin "cargo")
          rust-format-on-save t)))

(use-package clippy-flymake
  :vc (:url "https://git.sr.ht/~mgmarlow/clippy-flymake" :branch main)
  :hook (rust-mode . clippy-flymake-setup-backend))

(defun clippy-flymake-manually-activate-flymake ()
  "Shim for working around eglot's tendency to suppress flymake backends."
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (flymake-mode 1))

;; `eglot' by default will suppress all other flymake backends than its own
;; <https://github.com/joaotavora/eglot/issues/268> This workaround will
;; add `flymake-clippy'
(use-package eglot
  :ensure t
  :hook ((rust-mode . eglot-ensure)
         (eglot-managed-mode . clippy-flymake-manually-activate-flymake))
  :config
  (add-to-list 'eglot-stay-out-of 'flymake))

--------------------------------------------------------------


