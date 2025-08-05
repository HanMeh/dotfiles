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



