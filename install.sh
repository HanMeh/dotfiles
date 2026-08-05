#!/usr/bin/env bash

# Exit immediately if a command exits with a non-zero status
set -e

DOTFILES_DIR="$HOME/dotfiles"

echo "🚀 Starting dotfiles deployment..."

# 1. Detect package manager and install GNU Stow if missing
if ! command -v stow &> /dev/null; then
    echo "📦 GNU Stow is not installed. Attempting to install..."
    if command -v apt &> /dev/null; then
        sudo apt update && sudo apt install -y stow
    elif command -v pacman &> /dev/null; then
        sudo pacman -S --noconfirm stow
    elif command -v dnf &> /dev/null; then
        sudo dnf install -y stow
    elif command -v brew &> /dev/null; then
        brew install stow
    else
        echo "❌ Could not find a supported package manager (apt, pacman, dnf, brew)."
        echo "Please install 'stow' manually and rerun this script."
        exit 1
    fi
fi

# 2. Ensure the base XDG config folder exists
mkdir -p "$HOME/.config"

# 3. Change to the dotfiles directory
cd "$DOTFILES_DIR"

# List of folders (packages) to deploy
PACKAGES=(
    "alacritty"
    "emacs"
    "zellij"
    "fish"
    "hyprland"
    "bash"
)

# 4. Use Stow to symlink each package
echo "🔗 Creating symbolic links..."
for pkg in "${PACKAGES[@]}"; do
    if [ -d "$pkg" ]; then
        echo "   -> Stowing $pkg"
        # --restow ensures broken links are fixed, -v is for verbose output
        stow -R -v "$pkg"
    else
        echo "   ⚠️  Warning: Folder '$pkg' not found in dotfiles directory, skipping."
    fi
done

echo "✅ Dotfiles deployed successfully!"
