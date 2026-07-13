;;; 
;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; code
;; ====\
;; ====/
===============================================================================================================================
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))


;; (package-initialize) 

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

;; (horizontal-scroll-bar-mode -1)
;; (pixel-scroll-precision-mode 1)
;; (tab-bar-mode -1)
;; (column-number-mode)
;; (display-line-numbers-mmode)
;; (setq inhibit-startup-screen t)

;; Set Font (replace "FiraCode NF" with your preferred Nerd Font)
(set-face-attribute 'default nil :font "FiraCode NF-12" :weight 'regular)

;; 9. Icon Support for Nerd Fonts
(use-package nerd-icons
  :ensure t)
===============================================================================================================================
;; (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord t))

;; Emacs 29+ syntax
;; (keymap-global-set "C-t" #'whitespace-mode)
;; (keymap-set texinfo-mode-map "C-c C-c g" 'texinfo-insert-@group)
===============================================================================================================================
  (use-package rust-ts-mode
  :mode "\\.rs\\'"
  :init
;; If you prefer installing the community rust-mode package for its extra Cargo commands, use major-mode-remap-alist instead
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
;; Because rust-ts-mode is built-in but not associated with .rs files by default, you must explicitly link them using auto-mode-alist
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
;; (add-hook 'rust-ts-mode-hook #'eglot-ensure)
  :hook (rust-ts-mode . eglot-ensure)
  :config
  (setq rust-format-on-save t))

;; (add-hook 'rust-mode-hook 'cargo-minor-mode)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; (setq treesit-font-lock-level 4)
;; 4. Tree-sitter for modern Rust syntax highlighting (Emacs 29+)
(use-package treesit
  :init
  (setq major-mode-remap-alist
        '((rust-mode . rust-ts-mode))))

;; 5. LSP Mode & Rust Analyzer Setup
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (rust-ts-mode . lsp-deferred)
  :custom
  (lsp-rust-server 'rust-analyzer)
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.5)
  (lsp-keymap-prefix "C-c l"))


(use-package cargo
  :ensure t
  :hook (rust-ts-mode . cargo-minor-mode))

;; 6. Cargo Integration (With On-the-Fly Keybindings)
(use-package cargo
  :ensure t
  :hook (rust-ts-mode . cargo-minor-mode)
  :bind
  ;; Binds commands specifically inside Rust files
  (:map cargo-minor-mode-map
        ("C-c c b" . cargo-process-build)    ; Run cargo build
        ("C-c c t" . cargo-process-test)     ; Run cargo test
        ("C-c c r" . cargo-process-run)      ; Run cargo run
        ("C-c c x" . cargo-process-clean)))   ; Run cargo clean


;; (add-hook 'rust-ts-mode-hook #'eglot-ensure)
;; For IDE code intelligence (auto-complete, compiler diagnostics, and jump-to-definition), you should hook it into Emacs' built-in LSP client, Eglot
===============================================================================================================================
===============================================================================================================================
;; CHECKLIST
;; Install development tools (e.g., sudo apt install build-essential).
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
;; To install Nord Emacs manually, download the latest version or clone the repository. Afterwards copy the nord-theme.el file into the .emacs.d/themes folder located in your home directory.
;; External dynamic libraries (.so, .dll, or .dylib) compiled from Git repositories using treesit-language-source-alist.
 ------------------------------------------------------------------------------------------------------------------------------
  
;; TO DO
;; tree-sitter
;; lsp
;; dap
;; flycheck
;; cargo   | cargo-watch
;; git | git gutter
;; project management
;; toml
;; file manager
;; lint/format
;; use flycheck with lsp-mode
;; code completion
;; Rusty Object Notation (.ron) files
;; tab
;; psrenthesis/smartparens
;; bash-ts-mode
;; combobulate
;; documentation
;; ido-mode
;; ignore rules
;; minimap
 ------------------------------------------------------------------------------------------------------------------------------
  
;; comments
;; Font Locking: Tree-sitter supports different "levels" of highlighting. You can adjust this with (setq treesit-font-lock-level 4)
;; rust-ts-mode requires a compiled grammar. This treesit-auto downloads it for you.
;; Remap standard rust-mode to rust-ts-mode
;; to avoid :ensur t for every package add (setq use-package-always-ensure t)
;; Rust files start with #![...] (inner attributes), which looks like a shebang to Emacs -- prevent auto-chmod on save
;; CamelCase aware editing operations
:: (subword-mode +1)
;; rust-ts-mode is the built-in (since Emacs 29) major mode that uses Tree-sitter for advanced syntax highlighting and indentation.
;; You can use them together by configuring rustic-mode to use rust-ts-mode as its foundation rather than the older rust-mode.
;; If you ever dislike how rust-ts-mode indents a specific node layout (like match branches or closing brackets), you can inject custom layout logic directly into treesit-simple-indent-rules
;; Because Eglot hooks directly into the core language server, this requires rustfmt to be available alongside your system's rust-analyzer installation
;; sudo dnf install fira-code-fonts    and to verify    fc-list : family | grep -i "firacode"   or   fc-list : family | grep -i "firacode" | uniq
;; sudo dnf install ripgrep

 ------------------------------------------------------------------------------------------------------------------------------
  
(add-to-list 'treesit-language-source-alist
             '(rust "https://github.com/tree-sitter/tree-sitter-rust"))

;; After adding this to your configuration, run the setup command:M-x treesit-install-language-grammar → select rust           
 ------------------------------------------------------------------------------------------------------------------------------

;; 1. Configure the grammar source repository
(add-to-list 'treesit-language-source-alist
             '(rust "https://github.com/tree-sitter/tree-sitter-rust"))

;; 2. Auto-download the grammar if it is missing
(unless (treesit-language-available-p 'rust)
  (treesit-install-language-grammar 'rust))

;; 3. Bind .rs files to the native tree-sitter mode
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

;; 4. Hook up development utilities
(add-hook 'rust-ts-mode-hook
          (lambda ()
            (eglot-ensure)             ; Start LSP code intelligence
            (electric-pair-mode 1)     ; Auto-close brackets () [] {}
            (setq indent-tabs-mode nil))) ; Use spaces for indentation
 ------------------------------------------------------------------------------------------------------------------------------
;; 1. Tell Emacs where to find the Rust grammar repository
(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist
               '(rust "https://github.com")))

;; 2. Function to check and automatically install missing grammars
(defun my/ensure-treesit-grammars ()
  "Automatically install Rust tree-sitter grammar if missing."
  (interactive)
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
    (unless (treesit-language-available-p 'rust)
      (message "Tree-sitter: Installing 'rust' grammar...")
      (treesit-install-language-grammar 'rust))))

;; 3. Run the installer check on startup
(add-hook 'after-init-hook #'my/ensure-treesit-grammars)

;; 4. Automatically route all Rust files (.rs) to the tree-sitter mode
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))


 ------------------------------------------------------------------------------------------------------------------------------
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (rust-mode   . rust-ts-mode)
        (c-mode      . c-ts-mode)))

 ------------------------------------------------------------------------------------------------------------------------------
(add-hook 'rust-ts-mode-hook
          (lambda ()
            (setq-local rust-ts-mode-indent-offset 4) ; Controls standard code block indentation
            (setq-local tab-width 4)))

------------------------------------------------------------------------------------------------------------------------------

(defun my/eglot-format-buffer-on-save ()
  "Safely execute format-buffer via Eglot if the LSP server is active."
  (add-hook 'before-save-hook #'eglot-format-buffer nil t))

;; Automatically attach formatting behavior whenever rust-ts-mode initializes
(add-hook 'rust-ts-mode-hook #'my/eglot-format-buffer-on-save)


;; Increase Eglot's operational timeout window (Default is 0.5 seconds)
(setq eglot-connect-timeout 2.0)
------------------------------------------------------------------------------------------------------------------------------
(defun my/rust-ts-jump-to-next-match-arm ()
  "Instantly skip your cursor to the start of the next match arm node."
  (interactive)
  ;; Pass the exact string ID compiled from your grammar definition
  (treesit-search-forward "match_arm"))

(with-eval-after-load 'rust-ts-mode
  (define-key rust-ts-mode-map (kbd "C-c m") #'my/rust-ts-jump-to-next-match-arm))

------------------------------------------------------------------------------------------------------------------------------
;; 1. Define a central registry for a dozen common language repositories
(with-eval-after-load 'treesit
  (setq treesit-language-source-alist
        '((bash       "https://github.com")
          (c          "https://github.com")
          (c++        "https://github.compp")
          (cmake      "https://github.com")
          (css        "https://github.comss")
          (go         "https://github.com")
          (html       "https://github.com")
          (javascript "https://github.com" "master" "src")
          (json       "https://github.com")
          (python     "https://github.com")
          (rust       "https://github.com")
          (yaml       "https://github.com"))))

;; 2. Loop through the registry list and automatically install missing grammars
(defun my/ensure-all-treesit-grammars ()
  "Loop through the configured language list and compile missing binaries."
  (interactive)
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
    (dolist (mapping treesit-language-source-alist)
      (let ((lang (car mapping)))
        (unless (treesit-language-available-p lang)
          (message "Tree-sitter: Automatically installing '%s' grammar..." lang)
          (treesit-install-language-grammar lang))))))

;; 3. Run the automated installer check safely on startup
(add-hook 'after-init-hook #'my/ensure-all-treesit-grammars)

;; 4. Global fallback routing to translate legacy major modes to tree-sitter
(setq major-mode-remap-alist
      '((bash-mode       . bash-ts-mode)
        (c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (css-mode        . css-ts-mode)
        (go-mode         . go-ts-mode)
        (html-mode       . html-ts-mode)
        (js-mode         . js-ts-mode)
        (json-mode       . json-ts-mode)
        (python-mode     . python-ts-mode)
        (rust-mode       . rust-ts-mode)
        (yaml-mode       . yaml-ts-mode)))


------------------------------------------------------------------------------------------------------------------------------

bash
# 1. Create the user font directory if it doesn't exist
mkdir -p ~/.local/share/fonts

# 2. Download FiraCode Nerd Font directly from the official releases
wget -P ~/.local/share/fonts https://github.com

# 3. Move to the directory, extract the files, and clean up the zip
cd ~/.local/share/fonts
unzip FiraCode.zip
rm FiraCode.zip

# 4. Rebuild your system's font cache so Emacs can recognize it immediately
fc-cache -fv
------------------------------------------------------------------------------------------------------------------------------

;; press your LSP prefix (C-c l), a menu will pop up listing every available command
;; 8. Keybinding Hints for Learning LSP
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :custom
  ;; Delay in seconds before the popup menu appears (0.3s is fast but visible)
  (which-key-idle-delay 0.3)
  ;; Display the popup at the bottom of the frame
  (which-key-side-window-location 'bottom))

------------------------------------------------------------------------------------------------------------------------------
;; 5. LSP Mode & Rust Analyzer Setup (With Navigation Keybindings)
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (rust-ts-mode . lsp-deferred)
  :custom
  (lsp-rust-server 'rust-analyzer)
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.5)
  (lsp-keymap-prefix "C-c l")
  :bind
  ;; Custom navigation maps (M- is Alt/Meta, C- is Control)
  (:map lsp-mode-map
        ("M-."   . lsp-find-definition)       ; Jump to code definition
        ("M-,"   . xref-go-back)              ; Jump back to where you were
        ("M-?"   . lsp-find-references)       ; Find everywhere this is used
        ("C-c r" . lsp-rename)                ; Rename variable/function safely
        ("C-c a" . lsp-execute-code-action)   ; Trigger quick-fixes/imports
        ("C-c h" . lsp-describe-thing-at-point))) ; Show full documentation popup
------------------------------------------------------------------------------------------------------------------------------

;; 10. Real-time Syntax Checking with Flycheck
(use-package flycheck
  :ensure t
  :init 
  (global-flycheck-mode)
  :custom
  ;; Check for errors immediately when saving or changing lines
  (flycheck-check-syntax-automatically '(save mode-enabled newline))
  ;; Display errors down in the echo area quickly
  (flycheck-display-errors-delay 0.2))

;; Force LSP Mode to use Flycheck instead of Flymake
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  ;; Enable inline error squiggles
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  ;; Disable heavy UI tooltips to keep the editor minimalist and fast
  (lsp-ui-doc-enable nil))


;; How to use it in your workflow:Visual Cues: When you type a syntax or borrow-checker error, an underline will appear under the broken code.Quick Navigation: Use M-g n to jump to the next error and M-g p to jump to the previous error globally.Error List: Press C-c ! l to open a clean, split-pane bottom buffer listing every current warning and error across your workspace


------------------------------------------------------------------------------------------------------------------------------

;; 11. Project Management with Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind-keymap
  ;; Binds all project commands under the easy-to-remember prefix: C-c p
  ("C-c p" . projectile-command-map)
  :custom
  ;; Optimization: Use native indexing (git/find) for blazing-fast file caching
  (projectile-indexing-method 'alien)
  ;; Automatically find Cargo.toml files as project roots
  (projectile-project-search-path '("~/src/" "~/projects/")))


------------------------------------------------------------------------------------------------------------------------------

;; 12. Eye-Catching Icons in Dired File Manager
(use-package nerd-icons-dired
  :ensure t
  :hook
  ;; Automatically activate icons whenever you open a directory window
  (dired-mode . nerd-icons-dired-mode))

(use-package dired
  :hook
  ;; Hide ugly file permissions/owners by default so icons look clean (Press '(' to toggle)
  (dired-mode . dired-hide-details-mode))

------------------------------------------------------------------------------------------------------------------------------

;; 7. Modern, Minimalist Auto-Completion with Corfu
(use-package corfu
  :ensure t
  ;; Optional enrichment: adds icons to the completion popup
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)                 ; Enable auto-completion popups
  (corfu-auto-delay 0.2)         ; Fast popup response time
  (corfu-auto-prefix 2)          ; Trigger completion after typing 2 characters
  (corfu-cycle t)                ; Allow tab cycling to loop back to the top
  (corfu-quit-no-match 'separator) ; Smart quitting when typing doesn't match anything
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)     ; Tab moves down the list
        ("<tab>" . corfu-next)
        ("S-TAB" . corfu-previous) ; Shift-Tab moves up the list
        ("<backtab>" . corfu-previous)
        ("RET" . corfu-insert)))  ; Enter accepts the completion

;; 7b. Cape (Completion At Point Extensions) for LSP Integration
;; This bridges the gap between corfu and lsp-mode smoothly
(use-package cape
  :ensure t
  :init
  ;; Add useful completion functions to the global completion list
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))


(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

------------------------------------------------------------------------------------------------------------------------------
# To smoothly use the client from Alacritty, add quick commands to your shell configuration file (use ~/.bashrc if you use Bash, or ~/.zshrc if you use Zsh).
bash

# Open files inside your running Emacs background server instantly
alias ec="emacsclient -c -a ''"

# Open files directly inside your active Alacritty terminal window instead of a GUI window
alias ect="emacsclient -t -a ''"

# Set Emacs as your default system-wide editor for git commits, commands, etc.
export EDITOR="emacsclient -t -a ''"
------------------------------------------------------------------------------------------------------------------------------

;; 14. Smooth Background Format-on-Save with Apheleia
(use-package apheleia
  :ensure t
  :init
  (apheleia-global-mode +1))
------------------------------------------------------------------------------------------------------------------------------

;; 15. Custom High-Visibility Error Underlines for Light Themes
(custom-set-faces
 ;; Errors: Clear, solid crimson underline (Highly visible but soft)
 '(flycheck-error
   ((t (:underline (:color "#b00020" :style line) :weight bold))))
 
 ;; Warnings: Deep amber/ochre underline (Easy on the eyes, distinct from errors)
 '(flycheck-warning
   ((t (:underline (:color "#b26a00" :style line) :weight normal))))
 
 ;; Info/Hints: Clean slate blue underline for low-priority suggestions
 '(flycheck-info
   ((t (:underline (:color "#3949ab" :style line) :style dashed)))))

------------------------------------------------------------------------------------------------------------------------------


;; 17. Multi-Language Tree-Sitter & LSP Remapping
(with-eval-after-load 'treesit
  (setq major-mode-remap-alist
        (append '((rust-mode . rust-ts-mode)
                  (c++-mode  . c++-ts-mode)
                  (python-mode . python-ts-mode))
                major-mode-remap-alist)))

;; 18. Multi-Language LSP Hooks
;; Tells LSP Mode to automatically wake up for C++, Python, and Julia
(add-hook 'c++-ts-mode-hook #'lsp-deferred)
(add-hook 'python-ts-mode-hook #'lsp-deferred)

(use-package julia-mode
  :ensure t
  :hook (julia-mode . lsp-deferred))

;; 19. Ensure Apheleia Formats Everyone on Save
(with-eval-after-load 'apheleia
  ;; Maps formatters to your language major modes
  (setq apheleia-mode-alist
        (append '((rust-ts-mode . rustfmt)
                  (c++-ts-mode . clang-format)
                  (python-ts-mode . ruff)     ; or black
                  (julia-mode . julia-format))
                apheleia-mode-alist)))

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
