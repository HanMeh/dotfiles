#!/bin/bash

# Check if the network manager service is active to determine current state
if systemctl is-active --quiet NetworkManager; then
    echo "Turning off network stack for coding mode..."
    # Stop the core daemons immediately
    sudo systemctl stop NetworkManager systemd-resolved iwd
    echo "System is now dark. Enjoy your offline coding session!"
else
    echo "Waking up network daemons..."
    # Start services back up in the correct architectural order
    sudo systemctl start iwd systemd-resolved NetworkManager
    echo "Network services active. Re-negotiating secure lease..."
fi
