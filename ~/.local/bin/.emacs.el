;; Force Emacs to use the XDG standard .config directory
(setq user-emacs-directory (expand-file-name "~/.config/emacs/"))
(setq user-init-file (expand-file-name "init.el" user-emacs-directory))

;; Load the actual configuration from the XDG path
(load user-init-file)
