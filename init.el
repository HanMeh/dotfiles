;;; 

===============================================================================================================================
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
===============================================================================================================================
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
===============================================================================================================================
  (use-package rust-ts-mode
  :mode "\\.rs\\'"
  :init
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
  :hook (rust-ts-mode . eglot-ensure)
  :config
  (setq rust-format-on-save t))


(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))



(use-package cargo
  :ensure t
  :hook (rust-ts-mode . cargo-minor-mode))
===============================================================================================================================
===============================================================================================================================
;; CHECKLIST
;; XDG compatible ~/.emacs.d/init.el
;; rustup component add rust-analyzer
;; rustup component add rust-analyzer rustfmt clippy
;; systemctl --user enable emacs
;; export EDITOR = "emacsclient -t"
;; emacs --daemon
;; emacsclient -t (or -nw)
;; add(server-edit) to the init.el
;; if you use alacritty - use Nerd color palette
;; launch multiplexer inside terminal emulator  

 ------------------------------------------------------------------------------------------------------------------------------
  
;; TO DO
;; tree-sitter
;; lsp
;; dap
;; flycheck
;; cargo
;; git
;; project management
;; toml
;; file manager
;; lint/format
;; use flycheck with lsp-mode
;; code completion
 ------------------------------------------------------------------------------------------------------------------------------
  
;; comments
;; Font Locking: Tree-sitter supports different "levels" of highlighting. You can adjust this with (setq treesit-font-lock-level 4)
;; rust-ts-mode requires a compiled grammar. This treesit-auto downloads it for you.
;; Remap standard rust-mode to rust-ts-mode
;; to avoid :ensur t for every package add (setq use-package-always-ensure t)
 ------------------------------------------------------------------------------------------------------------------------------
  

