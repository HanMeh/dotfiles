#!/bin/bash

# Ensure the script runs as root
if [ "$EUID" -ne 0 ]; then
    echo "❌ Error: Please run this script with sudo or as root."
    exit 1
fi

LOG_FILE="/var/log/fedora_sway_optimization.log"
echo "=== Fedora Sway Optimization started at $(date) ===" | tee -a "$LOG_FILE"

# 1. Chassis and Battery Status Detection
CHASSIS_TYPE=$(hostnamectl chassis)
if [ -d /sys/class/power_supply/BAT0 ] || [ -d /sys/class/power_supply/BAT1 ] || [ "$CHASSIS_TYPE" = "laptop" ]; then
    IS_LAPTOP=true
    echo "🔋 System recognized as a Laptop running Sway." | tee -a "$LOG_FILE"
else
    IS_LAPTOP=false
    echo "🔌 System recognized as a Desktop PC running Sway." | tee -a "$LOG_FILE"
fi

# 2. Package Management & Cache Pruning
echo "🧹 Cleaning DNF package manager cache..." | tee -a "$LOG_FILE"
dnf clean all -y >> "$LOG_FILE" 2>&1

if command -v flatpak &> /dev/null; then
    echo "📦 Removing unused Flatpak runtimes..." | tee -a "$LOG_FILE"
    flatpak uninstall --unused -y >> "$LOG_FILE" 2>&1
fi

echo "📜 Compacting journald logs to 50MB..." | tee -a "$LOG_FILE"
journalctl --vacuum-size=50M >> "$LOG_FILE" 2>&1

# 3. Sway/Wayland-Specific Kernel & Performance Tuning (Sysctl)
echo "⚙️ Tuning system parameters for Sway/Wayland display pipeline..." | tee -a "$LOG_FILE"
# Lower swappiness to favor RAM, preserving zRAM snappiness under Sway
sysctl -w vm.swappiness=20 >> "$LOG_FILE" 2>&1
# Increase real-time scheduling priority limits (helps keep Sway rendering butter-smooth)
sysctl -w kernel.sched_rt_runtime_us=950000 >> "$LOG_FILE" 2>&1
# Boost memory maps limit (critical for Wine/Steam Proton gaming in Wayland)
sysctl -w vm.max_map_count=2147483642 >> "$LOG_FILE" 2>&1

# Save persistent sysctl definitions
cat << EOF > /etc/sysctl.d/99-fedora-sway.conf
vm.swappiness = 20
kernel.sched_rt_runtime_us = 950000
vm.max_map_count = 2147483642
fs.file-max = 2097152
EOF

# 4. Power & CPU Governor Optimization for Sway
if [ "$IS_LAPTOP" = true ]; then
    # Use TLP or standard cpupower if installed on Sway Spin
    if command -v tlp &> /dev/null; then
        echo "🔋 Triggering TLP power optimization update..." | tee -a "$LOG_FILE"
        tlp start >> "$LOG_FILE" 2>&1
    else
        ON_BATTERY=$(cat /sys/class/power_supply/AC*/online 2>/dev/null)
        if [ "$ON_BATTERY" = "0" ] && [ -f /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor ]; then
            echo "powersave" | tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor >> "$LOG_FILE" 2>&1
        elif [ -f /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor ]; then
            echo "schedutil" | tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor >> "$LOG_FILE" 2>&1
        fi
    fi
else
    # Force performance modes on Desktop hardware
    echo "🚀 Desktop hardware: Setting CPU governor to performance mode." | tee -a "$LOG_FILE"
    if [ -f /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor ]; then
        echo "performance" | tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor >> "$LOG_FILE" 2>&1
    fi
fi

# 5. Purge UI Cache & Stale Session Metadata
echo "🗑️ Flushing user caches and obsolete Sway log directories..." | tee -a "$LOG_FILE"
rm -rf /home/*/.cache/thumbnails/*
find /tmp -type f -atime +3 -delete >> "$LOG_FILE" 2>&1

echo "✅ Fedora Sway optimization complete at $(date) ===" | tee -a "$LOG_FILE"



nano ~/.config/environment.d/10-wayland.conf
MOZ_ENABLE_WAYLAND=1
ELECTRON_OZONE_PLATFORM_HINT=auto
_JAVA_AWT_WM_NONREPARENTING=1
sudo dnf install auto-cpufreq
sudo systemctl enable --now auto-cpufreq
# Example for a desktop setup
output * mode 2560x1440@144Hz
