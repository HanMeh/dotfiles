#!/bin/bash

# Target user account running the Sway session
USER_NAME=$(loginctl list-users | awk 'NR==2 {print $2}')
USER_ID=$(id -u "$USER_NAME")

# Get current RAM usage percentage
RAM_USAGE=$(free | grep Mem | awk '{print int($3/$2 * 100)}')

# If RAM crosses 85%, trigger a critical alert pop-up
if [ "$RAM_USAGE" -ge 85 ]; then
    # Use machine-uid to safely bridge root-level cron logic into user Dunst notifications
    sudo -u "$USER_NAME" DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/"$USER_ID"/bus \
    notify-send "🚨 CRITICAL MEMORY ALERT" "RAM is at ${RAM_USAGE}%! Click the Waybar module or hit Super+Alt+R to optimize." --urgency=critical
fi
