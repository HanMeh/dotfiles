#!/usr/bin/env bash

# ==============================================================================
# MINIMALIST TUI ENVIRONMENT AUTOMATED USB BACKUP SYSTEM
# ==============================================================================

# Define the absolute directory where your USB flash drive is mounted
USB_MOUNT="/mnt/usb"
BACKUP_DIR="$USB_MOUNT/dotfiles_backup"

# Define text coloring properties for clear terminal logging feedback
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}[*] Initializing TUI Environment Synchronization...${NC}"

# --- 1. SAFETY ENVIRONMENT CHECK ---
# Verify if the designated USB mount directory actively contains an attached storage device
if ! mountpoint -q "$USB_MOUNT"; then
    echo -e "${RED}[ERROR] Drive is not mounted at $USB_MOUNT.${NC}"
    echo -e "${YELLOW}[HINT] Run: sudo mount /dev/sdb1 $USB_MOUNT${NC}"
    exit 1
fi

# Ensure the targeted backup folder structure exists on the physical drive sectors
mkdir -p "$BACKUP_DIR"

# --- 2. DEFINE ARRAYS OF TARGET ITEMS ---
# Add paths relative to your $HOME directory that you want to mirror securely
TARGETS=(
    ".config/hypr"
    ".config/alacritty.toml"
    ".config/waybar"
    ".config/zellij"
    ".config/yazi"
    ".config/emacs/init.el"
    ".local/bin"
    ".bash_profile"
    ".bashrc"
)

echo -e "${YELLOW}[*] Syncing verified targets to storage pool...${NC}"

# --- 3. EXECUTE INCREMENTAL RSYNC ---
# Loop over each target path item and synchronize modifications recursively
for item in "${TARGETS[@]}"; do
    if [ -e "$HOME/$item" ]; then
        echo -e " -> Syncing: $item"
        
        # -a: Archive mode (preserves permissions, links, and times)
        # -r: Recursive parsing down directories
        # -u: Update mode (skips files that are newer on the receiver end)
        # --delete: Optional flag to delete files in backup if deleted at source
        rsync -aru --delete "$HOME/$item" "$BACKUP_DIR/"
    else
        echo -e "${RED} -> [WARNING] Target '$item' does not exist, skipping.${NC}"
    fi
done

# --- 4. FLUSH COMPILATION CACHES ---
echo -e "${YELLOW}[*] Flushing system block cache buffers to physical disk...${NC}"
sync

echo -e "${GREEN}[SUCCESS] Environment backup completed flawlessly!${NC}"
echo -e "${YELLOW}[*] Safe to unmount with: sudo umount $USB_MOUNT${NC}"
