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



