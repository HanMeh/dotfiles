#!/bin/bash

# Ensure the script runs as root
if [ "$EUID" -ne 0 ]; then
    echo "❌ Error: Please run this script with sudo or as root."
    exit 1
fi

LOG_FILE="/var/log/fedora_optimization.log"
echo "=== Fedora Workstation Optimization started at $(date) ===" | tee -a "$LOG_FILE"

# 1. Clean DNF Cache & System Metadata
echo "🧹 Cleaning DNF package manager cache..." | tee -a "$LOG_FILE"
dnf clean all -y >> "$LOG_FILE" 2>&1

# 2. Clean Flatpak Bloat (Unused runtimes and overrides)
if command -v flatpak &> /dev/null; then
    echo "📦 Removing unused Flatpak runtimes and data..." | tee -a "$LOG_FILE"
    flatpak uninstall --unused -y >> "$LOG_FILE" 2>&1
fi

# 3. Vacuum Systemd Journal Logs
echo "📜 Compacting journald logs to 50MB..." | tee -a "$LOG_FILE"
journalctl --vacuum-size=50M >> "$LOG_FILE" 2>&1

# 4. Optimize Desktop Responsiveness & Memory (Sysctl)
echo "⚙️ Tuning kernel parameters for Desktop UI responsiveness..." | tee -a "$LOG_FILE"
# Lower swappiness to favor RAM, but keep enough to balance Fedora's zRAM
sysctl -w vm.swappiness=30 >> "$LOG_FILE" 2>&1
# Maximize user file watch limits (fixes IDE/file-manager lag on large directories)
sysctl -w fs.file-max=2097152 >> "$LOG_FILE" 2>&1
# Boost memory maps limit (Required for Steam, Wine, and heavy gaming)
sysctl -w vm.max_map_count=2147483642 >> "$LOG_FILE" 2>&1

# Make desktop sysctl settings persistent
cat << EOF > /etc/sysctl.d/99-fedora-desktop.conf
vm.swappiness = 30
fs.file-max = 2097152
vm.max_map_count = 2147483642
EOF

# 5. Optimize CPU Power Management for Workstations
if command -v powerprofilesctl &> /dev/null; then
    echo "⚡ Setting system power profile to 'balanced' (good for laptops/desktops)..." | tee -a "$LOG_FILE"
    powerprofilesctl set balanced >> "$LOG_FILE" 2>&1
fi

# 6. Flush User and Thumbnail Cache (Safely targets user directories)
echo "🖼️ Clearing system-wide user thumbnail and cache items..." | tee -a "$LOG_FILE"
rm -rf /home/*/.cache/thumbnails/*
find /tmp -type f -atime +3 -delete >> "$LOG_FILE" 2>&1

echo "✅ Fedora Workstation optimization completed successfully at $(date) ===" | tee -a "$LOG_FILE"
