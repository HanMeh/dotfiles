#!/usr/bin/env bash

# Exit immediately if a command exits with a non-zero status
set -e

# Define modern XDG fallback paths
XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"

echo "Starting XDG migration script..."

# Function to safely move a config folder/file to XDG_CONFIG_HOME
migrate_config() {
    local old_path="$1"
    local new_subdir="$2"
    local target_dir="$XDG_CONFIG_HOME/$new_subdir"

    if [ -e "$old_path" ]; then
        echo "Found legacy path: $old_path"
        mkdir -p "$target_dir"
        
        # If it's a directory, move its contents. If it's a file, move the file.
        if [ -d "$old_path" ]; then
            echo "Moving contents to $target_dir/"
            cp -r "$old_path"/. "$target_dir/"
            rm -rf "$old_path"
        else
            echo "Moving file to $target_dir/"
            mv "$old_path" "$target_dir/"
        fi
        echo "Successfully migrated to $target_dir"
    fi
}

# 1. Migrate Sway configs (if using standard user paths)
migrate_config "$HOME/.sway" "sway"

# 2. Migrate Alacritty configs
migrate_config "$HOME/.alacritty.toml" "alacritty"
migrate_config "$HOME/.alacritty.yml" "alacritty"

# 3. Migrate Zellij configs
migrate_config "$HOME/.zellij" "zellij"

# 4. Migrate Yazi configs
migrate_config "$HOME/.yazi" "yazi"

# 5. Handle GNU Emacs specifically (The Legacy Redirect Trick)
if [ -d "$HOME/.emacs.d" ]; then
    echo "Found legacy Emacs directory: $HOME/.emacs.d"
    mkdir -p "$XDG_CONFIG_HOME/emacs"
    
    echo "Moving Emacs configuration files..."
    cp -r "$HOME/.emacs.d"/* "$XDG_CONFIG_HOME/emacs/" 2>/dev/null || true
    rm -rf "$HOME/.emacs.d"
    
    echo "Creating Emacs XDG redirect script at $HOME/.emacs.el..."
    cat << 'EOF' > "$HOME/.emacs.el"
;; Force Emacs to use the XDG standard .config directory
(setq user-emacs-directory (expand-file-name "~/.config/emacs/"))
(setq user-init-file (expand-file-name "init.el" user-emacs-directory))

;; Load the actual configuration from the XDG path
(load user-init-file)
EOF
    echo "Emacs migration complete."
fi

echo "Migration finished successfully! Your home directory is cleaner."
