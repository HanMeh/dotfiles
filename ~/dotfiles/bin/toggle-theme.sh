#!/usr/bin/env bash

# Accept 'dark' or 'light' as the first argument
MODE=$1

if [ "$MODE" != "dark" ] && [ "$MODE" != "light" ]; then
    echo "Usage: $0 [dark|light]"
    exit 1
fi

echo "🌙 Setting global theme mode to: $MODE"

# 1. Update Alacritty Theme Link
# Assumes you have theme files saved in ~/.config/alacritty/themes/
ln -sf "$HOME/.config/alacritty/themes/${MODE}.toml" "$HOME/.config/alacritty/current-theme.toml"

# 2. Update Hyprland Theme Link 
ln -sf "$HOME/.config/hypr/themes/${MODE}.conf" "$HOME/.config/hypr/current-theme.conf"
# Tell Hyprland to reload its config immediately
hyprctl reload &>/dev/null || true

# 3. Update Fish Theme Link
ln -sf "$HOME/.config/fish/themes/${MODE}.fish" "$HOME/.config/fish/current-theme.fish"



# Make it executable: chmod +x ~/dotfiles/bin/toggle-theme.sh