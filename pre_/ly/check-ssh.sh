#!/usr/bin/env bash

# Terminal text colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}=== SSH Infrastructure Diagnostic ===${NC}\n"

# 1. Check if the .ssh directory exists and has correct permissions
if [ -d "$HOME/.ssh" ]; then
    PERMS=$(stat -c "%a" "$HOME/.ssh")
    if [ "$PERMS" -eq 700 ]; then
        echo -e "[ ${GREEN}OK${NC} ] ~/.ssh directory exists with correct permissions (700)."
    else
        echo -e "[ ${YELLOW}WARN${NC} ] ~/.ssh exists but has permissions ($PERMS). Recommended is 700."
    fi
else
    echo -e "[ ${RED}FAIL${NC} ] ~/.ssh directory does not exist!"
    exit 1
fi

# 2. Check for private and public keys
PRIVATE_KEY="$HOME/.ssh/id_ed25519"
PUBLIC_KEY="$HOME/.ssh/id_ed25519.pub"

if [ -f "$PRIVATE_KEY" ] && [ -f "$PUBLIC_KEY" ]; then
    echo -e "[ ${GREEN}OK${NC} ] Cryptographic keypair (Ed25519) found."
    
    # Verify private key permissions (Must be 600 or SSH will refuse it)
    KEY_PERMS=$(stat -c "%a" "$PRIVATE_KEY")
    if [ "$KEY_PERMS" -eq 600 ]; then
        echo -e "[ ${GREEN}OK${NC} ] Private key has secure permissions (600)."
    else
        echo -e "[ ${RED}FAIL${NC} ] Private key permissions are too open ($KEY_PERMS)! Fix with: chmod 600 $PRIVATE_KEY"
    fi
else
    echo -e "[ ${YELLOW}INFO${NC} ] No Ed25519 keypair found at $PRIVATE_KEY. Checking config..."
fi

# 3. Check ~/.ssh/config for home-target alias
if [ -f "$HOME/.ssh/config" ]; then
    if grep -q "Host home-target" "$HOME/.ssh/config"; then
        echo -e "[ ${GREEN}OK${NC} ] 'home-target' configuration block found in ~/.ssh/config."
    else
        echo -e "[ ${RED}FAIL${NC} ] 'home-target' alias is missing from your SSH config file."
    fi
else
    echo -e "[ ${RED}FAIL${NC} ] ~/.ssh/config file does not exist!"
fi

# 4. Perform a non-interactive live connection test
echo -e "\n${YELLOW}Testing connection to home-target...${NC}"
# Connects, issues a true/false statement, and sets a 3-second timeout if server is down
ssh -o BatchMode=yes -o ConnectTimeout=3 home-target exit 2>/dev/null

if [ $? -eq 0 ]; then
    echo -e "[ ${GREEN}SUCCESS${NC} ] Perfect cryptographic handshake! Passwordless login to 'home-target' is operational."
else
    echo -e "[ ${RED}FAILED${NC} ] Public key connection failed. Possible reasons:"
    echo -e "         - The remote target is powered off or IP changed."
    echo -e "         - Your public key hasn't been added to the target's authorized_keys."
    echo -e "         - The remote SSH daemon is not running."
fi
