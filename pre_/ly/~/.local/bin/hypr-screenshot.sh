#!/bin/bash

# mkdir -p ~/.local/bin
# nano ~/.local/bin/hypr-screenshot.sh



# Ensure the screenshots folder exists
TARGET_DIR="$HOME/Pictures/Screenshots"
mkdir -p "$TARGET_DIR"

# Generate a filename using the current date and time
FILENAME="$TARGET_DIR/Screenshot_$(date +%Y-%m-%d_%H-%M-%S).png"

# Take the screenshot
# slurp lets you select an area, grim captures it, tee saves it and copies it to clipboard
slurp | grim -g - "$FILENAME"

# Check if the capture was successful (user didn't press Esc)
if [ -f "$FILENAME" ]; then
    # Copy the image file binary directly into the Wayland clipboard
    wl-copy < "$FILENAME"
    
    # Optional: Send a desktop notification (requires a notification daemon like mako/dunst)
    notify-send "Screenshot Captured" "Saved to $FILENAME and copied to clipboard." -i camera-photo
fi



# chmod 755 ~/.local/bin/hypr-screenshot.sh


#!/bin/bash

TARGET_DIR="$HOME/Pictures/Screenshots"
mkdir -p "$TARGET_DIR"
FILENAME="$TARGET_DIR/Screenshot_$(date +%Y-%m-%d_%H-%M-%S).png"

# Check if the user passed the "full" argument
if [ "$1" = "full" ]; then
    # Capture the entire screen instantly
    grim "$FILENAME"
else
    # Default behavior: Let the user select an area
    slurp | grim -g - "$FILENAME"
fi

if [ -f "$FILENAME" ]; then
    wl-copy < "$FILENAME"
    notify-send "Screenshot Captured" "Saved to $FILENAME" -i camera-photo
fi
