#!/bin/bash

# Exit immediately if any command fails
set -e

echo "=== 1. Installing Fedora Package Groups & Dependencies ==="
sudo dnf groupinstall -y "Development Tools" "C Development Tools and Libraries"
sudo dnf install -y \
    gtk3-devel gnutls-devel libtiff-devel giflib-devel \
    libjpeg-turbo-devel libpng-devel libXpm-devel ncurses-devel \
    jansson-devel libxml2-devel tree-sitter-devel libgccjit-devel

echo "=== 2. Cloning GNU Emacs Source Repository ==="
# Cleans up any existing emacs folder in the current directory to avoid conflicts
if [ -d "emacs" ]; then
    echo "Existing emacs directory found. Removing it for a fresh clone..."
    rm -rf emacs
fi
git clone --depth 1 --branch emacs-30 git://git.savannah.gnu.org/emacs.git
cd emacs

echo "=== 3. Running Autogen to build configuration scripts ==="
./autogen.sh

echo "=== 4. Configuring Emacs Build Flags (PGTK + Tree-Sitter + Native JIT) ==="
./configure --with-pgtk \
            --with-native-compilation=aot \
            --with-tree-sitter \
            --with-modules

echo "=== 5. Compiling Emacs (Using all CPU cores) ==="
make -j$(nproc)

echo "=== 6. Installing Emacs System-wide ==="
sudo make install

echo "=== 7. Verifying Installation ==="
emacs --version

echo "========================================="
echo " 🎉 Emacs build completed successfully! "
echo "========================================="








# chmod +x build_emacs.sh
# ./build_emacs.sh

# Example: Install grammars for C, Python, and Bash
sudo dnf install -y tree-sitter-c tree-sitter-python tree-sitter-bash

# Install the language intelligence server
rustup component add rust-analyzer

# Download the official compiler source files (Mandatory for jumping to standard library code definitions)
rustup component add rust-src

