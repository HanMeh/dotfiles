-- Define your default applications
local mainMod = "SUPER"
local myTerminal = "kitty"
local myRunner = "rofi -show drun"

-- Map a keybind using your variables
hl.bind(mainMod .. " + T", hl.dsp.exec_cmd(myTerminal))
hl.bind(mainMod .. " + D", hl.dsp.exec_cmd(myRunner))




-- Keep track of the current layout state
local isCompactMode = false

-- Bind a key to run a custom Lua toggle function
hl.bind("SUPER + G", function()
    if isCompactMode then
        -- Restore spacious gaps
        hl.config({ ["general:gaps_in"] = 5, ["general:gaps_out"] = 20 })
        isCompactMode = false
    else
        -- Shrink gaps to maximize screen space
        hl.config({ ["general:gaps_in"] = 0, ["general:gaps_out"] = 0 })
        isCompactMode = true
    end
end)



-- Loop through workspaces 1 through 9
for i = 1, 9 do
    -- SUPER + [1-9] -> Switch to workspace
    hl.bind("SUPER + " .. i, hl.dsp.workspace(tostring(i)))
    
    -- SUPER + SHIFT + [1-9] -> Move active window to workspace
    hl.bind("SUPER + SHIFT + " .. i, hl.dsp.window.movetoworkspace(tostring(i)))
end



hl.on("hyprland.start", function ()
    -- Native Security and Audio services
    hl.exec_cmd("systemctl --user start hyprpolkitagent")
    hl.exec_cmd("pipewire")
    hl.exec_cmd("wireplumber")
    
    -- Visual interface components
    hl.exec_cmd("waybar")
    hl.exec_cmd("mako")
end)


hl.bind("SUPER + A", hl.dsp.exec_cmd("pavucontrol"))








-- ==========================
-- Modern Hyprland Window Rules Configuration
hl.config({
    windowrule = {
        -- 1. Force utility apps to always open in FLOATING mode
        "float, ^(org.pulseaudio.pavucontrol)$",
        "float, ^(blueman-manager)$",
        "float, ^(nm-connection-editor)$",

        -- 2. Force heavy apps to stick to SPECIFIC WORKSPACES
        -- Assign Web Browsers (e.g., Firefox/Chromium) permanently to Workspace 2
        "workspace 2, ^(firefox)$",
        "workspace 2, ^(chromium-browser)$",
        
        -- Assign Communication apps (e.g., Discord/Slack) permanently to Workspace 3
        "workspace 3, ^(discord)$",

        -- 3. Define Window Geometric Constraints (Size & Centering)
        -- Forces your audio mixer to be centered and look like a neat popup card
        "size 700 450, ^(org.pulseaudio.pavucontrol)$",
        "center, ^(org.pulseaudio.pavucontrol)$",

        -- 4. Aesthetic Overrides (Opacity & Blur)
        -- Make your terminal slightly transparent when active (95%) and inactive (85%)
        "opacity 0.95 0.85, ^(kitty)$"
    }
})




-- ============================
hl.config({
    -- Select your preferred master layout engine
    general = {
        layout = "dwindle", -- Options: "dwindle" or "master"
        gaps_in = 5,        -- Gaps between windows
        gaps_out = 10,      -- Gaps between windows and monitor edges
    },

    -- Configuration specific to the Dwindle layout
    dwindle = {
        pseudotile = true,     -- Master switch for pseudotiling
        preserve_split = true, -- Guarantees the split direction stays locked when moving windows
        force_split = 2,       -- Forces new splits to always open to the right (0: mouse, 1: left, 2: right)
    },

    -- Configuration specific to the Master layout
    master = {
        new_status = "master", -- New windows automatically become the primary master window
        mfact = 0.60,          -- The master window takes up exactly 60% of the screen width
    }
})



-- ===========================

hl.config({
    windowrulev2 = {
        "float, class:^(kitty-popup)$",
        "size 800 500, class:^(kitty-popup)$",
        "center, class:^(kitty-popup)$",
        "pin, class:^(kitty-popup)$", -- Keeps it visible across all workspaces
    }
})

-- ============================
hl.config({
    workspace = {
        -- 'w[tv1]' means: when there is only 1 tiled window visible
        "w[tv1], gapsout:0, gapsin:0",
        "f[1], gapsout:0, gapsin:0",
    },
    windowrulev2 = {
        -- Removes window borders entirely when a single window maximizes to the edges
        "noborder, onworkspace:w[tv1]",
        "rounding 0, onworkspace:w[tv1]",
    }
})

-- =================================
hl.config({
    windowrulev2 = {
        "float, title:^(Picture-in-Picture)$",
        "pin, title:^(Picture-in-Picture)$",
        "size 400 225, title:^(Picture-in-Picture)$",
        "move 100%-420 100%-245, title:^(Picture-in-Picture)$", -- Snaps it perfectly to the bottom-right corner
    }
})

-- ======================================
-- Toggle the current active window between Tiled and Floating modes
hl.bind("SUPER", "Space", hl.dsp.window.togglefloating())

-- Toggle the active window into true fullscreen mode
hl.bind("SUPER", "F", hl.dsp.window.fullscreen())

-- Pseudo-tile toggle (keeps a floating app constrained within the tiling tile grid)
hl.bind("SUPER", "P", hl.dsp.window.togglepseudotile())

-- ========================================
