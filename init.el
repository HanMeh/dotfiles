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



