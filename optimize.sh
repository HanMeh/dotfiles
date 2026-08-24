#!/bin/bash

# linux automated system optimization script


# Ensure the script runs as root
if [ "$EUID" -ne 0 ]; then
    echo "❌ Error: Please run this script with sudo or as root."
    exit 1
fi

# Define log file
LOG_FILE="/var/log/system_optimization.log"
echo "=== Optimization started at $(date) ===" | tee -a "$LOG_FILE"

# 1. Clean Package Manager Cache & Bloat
echo "🧹 Cleaning package manager cache..." | tee -a "$LOG_FILE"
if [ -f /usr/bin/apt ]; then
    # Debian/Ubuntu systems
    apt-get update -y >> "$LOG_FILE" 2>&1
    apt-get autoremove -y >> "$LOG_FILE" 2>&1
    apt-get autoclean -y >> "$LOG_FILE" 2>&1
    apt-get clean -y >> "$LOG_FILE" 2>&1
elif [ -f /usr/bin/dnf ]; then
    # RHEL/CentOS/Fedora systems
    dnf autoremove -y >> "$LOG_FILE" 2>&1
    dnf clean all -y >> "$LOG_FILE" 2>&1
fi

# 2. Vacuum Systemd Journal Logs
echo "📜 Vacuuming systemd journal logs to 100MB..." | tee -a "$LOG_FILE"
journalctl --vacuum-size=100M >> "$LOG_FILE" 2>&1

# 3. Optimize Virtual Memory Settings (sysctl)
echo "⚙️ Tweaking kernel virtual memory parameters..." | tee -a "$LOG_FILE"
# Reduce swappiness (forces system to use RAM more efficiently before swapping)
sysctl -w vm.swappiness=10 >> "$LOG_FILE" 2>&1
# Increase system file descriptor limits
sysctl -w fs.file-max=2097152 >> "$LOG_FILE" 2>&1
# Optimize directory structures caching
sysctl -w vm.vfs_cache_pressure=50 >> "$LOG_FILE" 2>&1

# Make sysctl settings persistent across reboots
cat << EOF > /etc/sysctl.d/99-optimization.conf
vm.swappiness = 10
fs.file-max = 2097152
vm.vfs_cache_pressure = 50
EOF

# 4. Safe Clear of PageCache, Dentries, and Inodes
echo "🧠 Safely freeing up unallocated memory (PageCache)..." | tee -a "$LOG_FILE"
sync; echo 3 > /proc/sys/vm/drop_caches

# 5. Clean /tmp Directory (Files older than 7 days)
echo "🗑️ Removing old temporary files..." | tee -a "$LOG_FILE"
find /tmp -type f -atime +7 -delete >> "$LOG_FILE" 2>&1

echo "✅ Optimization completed successfully at $(date) ===" | tee -a "$LOG_FILE"
