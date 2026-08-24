#!/bin/bash

# Ensure the script runs as root
if [ "$EUID" -ne 0 ]; then
    echo "❌ Error: Please run this script with sudo or as root."
    exit 1
fi

LOG_FILE="/var/log/sway_ram_optimization.log"
echo "=== RAM Optimization Started at $(date) ===" | tee -a "$LOG_FILE"

# 1. Optimize zRAM (Fedora's built-in compressed swap-in-RAM)
# We change the swappiness to 150. This forces the system to compress idle pages 
# into zRAM aggressively, leaving maximum physical RAM free for active apps.
echo "⚡ Tuning kernel to use zRAM compression aggressively..." | tee -a "$LOG_FILE"
sysctl -w vm.swappiness=150 >> "$LOG_FILE" 2>&1

# Lower the page cache retention to release memory from closed files faster
sysctl -w vm.vfs_cache_pressure=150 >> "$LOG_FILE" 2>&1

# Save persistently
cat << EOF > /etc/sysctl.d/99-ram-optimization.conf
vm.swappiness = 150
vm.vfs_cache_pressure = 150
EOF

# 2. Re-initialize and Compact zRAM Swap Pages
echo "🌀 Compacting active zRAM pools..." | tee -a "$LOG_FILE"
if [ -d /sys/block/zram0 ]; then
    echo 1 > /sys/block/zram0/compact
fi

# 3. Clean System Cache & Drop Freeable Buffers Safely
echo "🧠 Safely purging freeable page and inode caches..." | tee -a "$LOG_FILE"
sync; echo 3 > /proc/sys/vm/drop_caches

# 4. Terminate Orphaned/Zombied Flatpak and User Processes
echo "🧹 Reclaiming RAM from background or dangling processes..." | tee -a "$LOG_FILE"
if command -v flatpak &> /dev/null; then
    flatpak killall >> "$LOG_FILE" 2>&1
fi

echo "✅ RAM Optimization Complete at $(date) ===" | tee -a "$LOG_FILE"
