# Add this to the bottom of your ~/.bash_profile
if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ]; then
  exec Hyprland
fi


---------------------------------------------------

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ]; then
  if [ "$XDG_VTNR" -eq 1 ]; then
    # Logging into TTY1: Launches Hyprland with your standard Yazi + Emacs split layout
    export HYPRLAND_START_CMD="alacritty -e zellij --layout coder"
    exec Hyprland
  elif [ "$XDG_VTNR" -eq 2 ]; then
    # Logging into TTY2: Launches a separate Hyprland instance with the full-screen Emacs layout
    export HYPRLAND_START_CMD="alacritty -e zellij --layout editor_only"
    exec Hyprland
  fi
fi
------------------------------------------------------
# If we are in a local terminal session and on TTY1, execute Hyprland automatically
if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ]; then
  XDG_CURRENT_DESKTOP=Hyprland exec Hyprland
fi
-----------------------------------------------------
