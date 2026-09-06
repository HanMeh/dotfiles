#!/usr/bin/env bash

# 1. Ensure any old matching zellij sessions are cleared out
zellij kill-session presentation-session 2>/dev/null

# 2. Launch the session using our custom layout and give it a static name
# So that ttyd can look up and attach to this exact session automatically
zellij --session presentation-session --layout broadcast.kdl
