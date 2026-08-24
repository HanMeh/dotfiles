#!/bin/bash

# Ensure the script runs as root
if [ "$EUID" -ne 0 ]; then
    echo "❌ Error: Please run this script with sudo or as root."
    exit 1
fi

LOG_FILE="/var/log/fedora_hybrid_optimization.log"
echo "=== Fedora Hybrid Optimization started at $(date) ===" | tee -a "$LOG_FILE"

# 1. Hardware Detection (Laptop vs. Desktop)
CHASSIS_TYPE=$(hostnamectl chassis)
echo "🖥️ Detected Hardware Chassis Type: $CHASSIS_TYPE" | tee -a "$LOG_FILE"

# Check battery presence
if [ -d /sys/class/power_supply/BAT0 ] || [ -d /sys/class/power_supply/BAT1 ] || [ "$CHASSIS_TYPE" = "laptop" ]; then
    IS_LAPTOP=true
    echo "🔋 System recognized as a Laptop." | tee -a "$LOG_FILE"
else
    IS_LAPTOP=false
    echo "🔌 System recognized as a Desktop PC." | tee -a "$LOG_FILE"
fi

# 2. Package & Bloat Cleanup (Universal)
echo "🧹 Cleaning DNF package manager cache..." | tee -a "$LOG_FILE"
dnf clean all -y >> "$LOG_FILE" 2>&1

if command -v flatpak &> /dev/null; then
    echo "📦 Removing unused Flatpak runtimes..." | tee -a "$LOG_FILE"
    flatpak uninstall --unused -y >> "$LOG_FILE" 2>&1
fi

echo "📜 Compacting journald logs to 50MB..." | tee -a "$LOG_FILE"
journalctl --vacuum-size=50M >> "$LOG_FILE" 2>&1

# 3. Dynamic Power & CPU Performance Tuning
if command -v powerprofilesctl &> /dev/null; then
    if [ "$IS_LAPTOP" = true ]; then
        # Check if currently on battery or AC power
        ON_BATTERY=$(cat /sys/class/power_supply/AC*/online 2>/dev/null)
        
        if [ "$ON_BATTERY" = "0" ]; then
            echo "🔋 Laptop is running on BATTERY. Setting profile to 'power-saver'." | tee -a "$LOG_FILE"
            powerprofilesctl set power-saver >> "$LOG_FILE" 2>&1
            # Aggressive disk write-backs to save battery power
            sysctl -w vm.dirty_writeback_centisecs=1500 >> "$LOG_FILE" 2>&1
        else
            echo "🔌 Laptop is PLUGGED IN. Setting profile to 'balanced'." | tee -a "$LOG_FILE"
            powerprofilesctl set balanced >> "$LOG_FILE" 2>&1
            sysctl -w vm.dirty_writeback_centisecs=500 >> "$LOG_FILE" 2>&1
        fi
    else
        # Force performance or high balanced on pure Desktop systems
        echo "🚀 Desktop PC detected. Unleashing 'performance' profile." | tee -a "$LOG_FILE"
        powerprofilesctl set performance >> "$LOG_FILE" 2>&1
        sysctl -w vm.dirty_writeback_centisecs=500 >> "$LOG_FILE" 2>&1
    fi
fi

# 4. Global Workstation System Tweaks (Sysctl)
echo "⚙️ Tweaking memory limits for gaming and multi-tasking..." | tee -a "$LOG_FILE"
sysctl -w vm.swappiness=30 >> "$LOG_FILE" 2>&1
sysctl -w fs.file-max=2097152 >> "$LOG_FILE" 2>&1
sysctl -w vm.max_map_count=2147483642 >> "$LOG_FILE" 2>&1

# Save basic system rules persistently
cat << EOF > /etc/sysctl.d/99-fedora-desktop.conf
vm.swappiness = 30
fs.file-max = 2097152
vm.max_map_count = 2147483642
EOF

# 5. Flush Temporary Assets & Thumbnail Logs
echo "🖼️ Clearing system-wide user thumbnail caches..." | tee -a "$LOG_FILE"
rm -rf /home/*/.cache/thumbnails/*
find /tmp -type f -atime +3 -delete >> "$LOG_FILE" 2>&1

echo "✅ Optimization routine completed successfully at $(date) ===" | tee -a "$LOG_FILE"
