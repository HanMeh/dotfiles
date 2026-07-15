# 🖥️ Screen-Aware Zellij Auto-Loader with Rotation Detection
zellij_auto_load() {
    if [ -f ".zellij-layout.kdl" ]; then
        local session_name=$(basename "$PWD")

        # 1. Detect if any active screen is in Portrait mode (Height > Width)
        # Works out of the box on X11 and most standard Wayland environments via xrandr
        local is_portrait=false
        if command -v xrandr >/dev/null 2>&1; then
            # Extract resolution of primary or active connected display (e.g. 1080x1920)
            local res=$(xrandr --current | grep " connected" | grep -o '[0-9]\+x[0-9]\+' | head -n 1)
            local width=$(echo "$res" | cut -d'x' -f1)
            local height=$(echo "$res" | cut -d'x' -f2)
            
            if [ -n "$height" ] && [ -n "$width" ] && [ "$height" -gt "$width" ]; then
                is_portrait=true
            fi
        fi

        # 2. Manage Session Loading or Attachment
        if zellij list-sessions 2>/dev/null | grep -q "$session_name"; then
            notify-send -t 3000 -u low -i "terminal" "Zellij Workspace" \
                "🔄 Attaching to active session: <b>$session_name</b>"
            zellij attach "$session_name"
        else
            # If portrait layout is detected, target your vertical-ide file
            if [ "$is_portrait" = true ]; then
                notify-send -t 4000 -u normal -i "emacs" "Zellij Workspace" \
                    "📱 Vertical Monitor Detected! Launching portrait stack for: <b>$session_name</b>"
                
                # Loads your specialized vertical configuration layout
                zellij --layout ~/.config/zellij/layouts/vertical-ide.kdl --session "$session_name"
            else
                notify-send -t 4000 -u normal -i "emacs" "Zellij Workspace" \
                    "🖥️ Widescreen Detected! Launching production grid for: <b>$session_name</b>"
                
                # Fallback to your local horizontal three-pane grid layout
                zellij --layout .zellij-layout.kdl --session "$session_name"
            fi
        fi
    fi
}



# Wayland alternative using standard sysfs monitor card geometry readings
local width=$(cat /sys/class/drm/card*-*/modes 2>/dev/null | head -n 1 | cut -d'x' -f1)
local height=$(cat /sys/class/drm/card*-*/modes 2>/dev/null | head -n 1 | cut -d'x' -f2)


# Add this to your ~/.bashrc or ~/.zshrc
ze-emacs() {
    # Dynamically rename the current Zellij pane to show it is running Emacs
    zellij action rename-pane "Emacs: $(basename "$PWD")"
    emacsclient -nw "$@"
}
