#!/usr/bin/env bash

# ==============================================================================
# FEDORA / RHEL SYSTEM OPTIMIZATION & CLEANUP SCRIPT
# ==============================================================================
# This script disables aggressive background processes, telemetry trackers, 
# unnecessary logging writes, and unoptimized repository syncing cycles.
# ==============================================================================

# Ensure the script is running as root
if [ "$EUID" -ne 0 ]; then
    echo "[-] Error: This script must be executed with sudo or root privileges." >&2
    exit 1
fi

echo "[+] Starting Fedora/RHEL system optimization sequence..."
echo "--------------------------------------------------------"

# ------------------------------------------------------------------------------
# 1. OPTIMIZE ENGINE CONFIGURATIONS (DNF / DNF5)
# ------------------------------------------------------------------------------
# Configures local metadata caching safety, reduces kernel clutter, turns on
# DeltaRPMs, and disables the 'Countme' analytics tracker tracker.
# ------------------------------------------------------------------------------
DNF_CONF="/etc/dnf/dnf.conf"

if [ -f "$DNF_CONF" ]; then
    echo "[*] Optimizing $DNF_CONF parameters..."
    
    # Backup original configuration
    cp "$DNF_CONF" "${DNF_CONF}.bak"
    
    # Remove existing instances of parameters we want to override cleanly
    sed -i '/^metadata_expire=/d' "$DNF_CONF"
    sed -i '/^metadata_timer_sync=/d' "$DNF_CONF"
    sed -i '/^countme=/d' "$DNF_CONF"
    sed -i '/^installonly_limit=/d' "$DNF_CONF"
    sed -i '/^deltarpm=/d' "$DNF_CONF"

    # Append optimized variables inside the main configuration block
    cat << 'EOF' >> "$DNF_CONF"
# Performance & Slow Network Overrides
metadata_expire=14d
metadata_timer_sync=0
countme=false
installonly_limit=2
deltarpm=1
EOF
    echo "[+] DNF/DNF5 profiles successfully hardened and optimized."
else
    echo "[-] Warning: $DNF_CONF not found. Skipping engine text overrides."
fi

# ------------------------------------------------------------------------------
# 2. SILENCE AUTOMATIC REPOSITORY SYNC TIMERS
# ------------------------------------------------------------------------------
# Permanently deactivates background dnf-makecache timers to stop sudden 
# CPU and memory spikes during active computer usage.
# ------------------------------------------------------------------------------
echo "[*] Disabling background repository cache timers..."

# Disable DNF4 legacy background timers
systemctl disable --now dnf-makecache.timer >/dev/null 2>&1

# Disable modern DNF5 background timers
systemctl disable --now dnf5-makecache.timer >/dev/null 2>&1

echo "[+] Automated package manager background caching cycles deactivated."

# ------------------------------------------------------------------------------
# 3. MASK PACKAGEKIT BACKEND DAEMON
# ------------------------------------------------------------------------------
# Stops background database locking and reclaims system idle memory. 
# ------------------------------------------------------------------------------
echo "[*] Neutralizing PackageKit backend daemon..."

systemctl stop packagekit.service >/dev/null 2>&1
systemctl disable packagekit.service >/dev/null 2>&1
systemctl mask packagekit.service >/dev/null 2>&1

echo "[+] PackageKit successfully masked. Terminal commands take precedence."

# ------------------------------------------------------------------------------
# 4. DEACTIVATE SYSTEMD JOURNALD CORE DUMPS
# ------------------------------------------------------------------------------
# Prevents disk write spikes during app crashes and saves storage drive space.
# ------------------------------------------------------------------------------
COREDUMP_CONF="/etc/systemd/coredump.conf"

if [ -f "$COREDUMP_CONF" ]; then
    echo "[*] Disabling systemd crash core dump retention storage..."
    cp "$COREDUMP_CONF" "${COREDUMP_CONF}.bak"
    
    sed -i '/^Storage=/d' "$COREDUMP_CONF"
    
    cat << 'EOF' >> "$COREDUMP_CONF"
[Coredump]
Storage=none
EOF
    systemctl daemon-reload
    echo "[+] Core dump disk-write rules disabled."
else
    echo "[-] Warning: $COREDUMP_CONF missing. Skipping."
fi

# ------------------------------------------------------------------------------
# 5. TURN OFF NETWORKMANAGER CONNECTIVITY PINGING
# ------------------------------------------------------------------------------
# Removes unsolicited heartbeat network traffic sent out to Fedora domain checkers.
# ------------------------------------------------------------------------------
NM_CONF="/etc/NetworkManager/NetworkManager.conf"

if [ -f "$NM_CONF" ]; then
    echo "[*] Stripping NetworkManager captive portal analytics checking..."
    cp "$NM_CONF" "${NM_CONF}.bak"
    
    # Remove existing connectivity sections if present
    sed -i '/\[connectivity\]/,/^$/d' "$NM_CONF"
    
    cat << 'EOF' >> "$NM_CONF"

[connectivity]
enabled=false
EOF
    systemctl restart NetworkManager
    echo "[+] NetworkManager diagnostic pings set to off."
else
    echo "[-] Warning: $NM_CONF missing. Skipping connectivity configuration."
fi

# ------------------------------------------------------------------------------
# 6. CLEAR CACHES AND CONSOLIDATE STATE
# ------------------------------------------------------------------------------
echo "[*] Purging obsolete, corrupted caches and rebuilding local indexes..."
dnf clean all >/dev/null 2>&1
rm -rf /var/cache/dnf/* /var/cache/libdnf5/*

echo "--------------------------------------------------------"
echo "[+] SYSTEM OPTIMIZATION COMPLETE!"
echo "[!] Note: To update your system from now on, run: 'sudo dnf upgrade --refresh'"


# ------------------------------------------------------------------------------
# EXTRA: EMERGENCY OLD KERNEL PURGE
# ------------------------------------------------------------------------------
echo "[*] Auditing installed kernels to free up /boot partition space..."
RUNNING_KERNEL=$(uname -r)
# Query all installed kernel cores except the active one
OLD_KERNELS=$(rpm -q kernel-core | grep -v "$RUNNING_KERNEL")

if [ ! -z "$OLD_KERNELS" ]; then
    echo "[+] Found old kernels. Executing safe reduction routine..."
    # Safely passes historical instances out to the automated dnf removal logic
    dnf remove -y --oldpackage $OLD_KERNELS >/dev/null 2>&1
    echo "[+] Obsolete kernel structures dropped. Boot space clear."
else
    echo "[+] No legacy kernels detected. Boot layout is already optimized."
fi
