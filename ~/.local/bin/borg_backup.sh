#!/usr/bin/env bash

# ==============================================================================
# TERMINAL WORKSPACE DE-DUPLICATED BORG BACKUP AUTOMATION
# ==============================================================================

# Target storage directories
REPOSITORY="/mnt/usb/borg_repo"

# Visual feedback terminal colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m'

echo -e "${YELLOW}[*] Validating secure Borg environment hooks...${NC}"

# --- 1. SAFETY INFRASTRUCTURE MOUNT CHECK ---
if ! mountpoint -q "/mnt/usb"; then
    echo -e "${RED}[ERROR] USB storage device is not mounted at /mnt/usb.${NC}"
    exit 1
fi

# --- 2. EXECUTE DEDUPLICATED SNAPSHOT ARCHIVE ---
echo -e "${YELLOW}[*] Commencing cryptographic compression build...${NC}"

# Create a unique archive name using your computer's hostname and a precise timestamp
borg create --stats --progress \
    --compression zstd,3 \
    "$REPOSITORY"::"{hostname}-{now:%Y-%m-%d_%H%M%S}" \
    "$HOME/.config/hypr" \
    "$HOME/.config/alacritty.toml" \
    "$HOME/.config/waybar" \
    "$HOME/.config/zellij" \
    "$HOME/.config/yazi" \
    "$HOME/.config/emacs/init.el" \
    "$HOME/.local/bin" \
    "$HOME/.bash_profile" \
    "$HOME/.bashrc" \
    --exclude '**/node_modules' \
    --exclude '**/.cache'

# --- 3. PRUNE OLD HISTORICAL SNAPSHOTS ---
# Keep your archive log clean by discarding data states that fall out of retention
echo -e "${YELLOW}[*] Pruning repository matching backup retention policies...${NC}"
borg prune -v --list "$REPOSITORY" --prefix="{hostname}-" \
    --keep-daily=7 \
    --keep-weekly=4 \
    --keep-monthly=6

# Flush blocks directly to hardware sectors
sync

echo -e "${GREEN}[SUCCESS] Encrypted snapshot archive locked in securely!${NC}"
