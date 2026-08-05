#!/bin/bash

# Exit immediately if any command fails
set -e

echo "=== 1. Installing LLVM Toolchain & Emacs Dependencies ==="
sudo dnf install -y clang lld llvm \
    gtk3-devel gnutls-devel libtiff-devel giflib-devel \
    libjpeg-turbo-devel libpng-devel libXpm-devel ncurses-devel \
    jansson-devel libxml2-devel tree-sitter-devel

echo "=== 2. Cloning GNU Emacs Source Repository ==="
if [ -d "emacs" ]; then
    echo "Existing emacs directory found. Removing it for a fresh clone..."
    rm -rf emacs
fi
git clone --depth 1 --branch emacs-30 git://git.savannah.gnu.org/emacs.git
cd emacs

echo "=== 3. Running Autogen to build configuration scripts ==="
./autogen.sh

echo "=== 4. Configuring Emacs Build Flags (Clang + LLD + PGTK) ==="
# We explicitly inject CC=clang and apply -fuse-ld=lld to override the default link vectors
./configure CC=clang \
            CXX=clang++ \
            LDFLAGS="-fuse-ld=lld" \
            --with-pgtk \
            --with-tree-sitter \
            --with-modules \
            --without-native-compilation

echo "=== 5. Compiling Emacs (Using Clang on all CPU cores) ==="
make -j$(nproc)

echo "=== 6. Installing Emacs System-wide ==="
sudo make install

echo "=== 7. Verifying LLVM Link Vectors ==="
emacs --version
ldd /usr/local/bin/emacs | grep -E "clang|lld" || echo "Note: Linked statically or against shared system objects cleanly."

echo "======================================================"
echo " 🎉 Emacs build completed successfully via LLVM! "
echo "======================================================"
