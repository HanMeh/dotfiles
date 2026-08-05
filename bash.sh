# Install core development tools and compilation environments
sudo dnf groupinstall -y "Development Tools"
sudo dnf install -y autoconf automake texinfo git make gcc g++ \
    gtk3-devel gnutls-devel libtiff-devel giflib-devel \
    libjpeg-turbo-devel libpng-devel libXpm-devel ncurses-devel \
    jansson-devel libxml2-devel tree-sitter-devel

# Install the GCC JIT development package for lightning-fast Emacs Lisp compilation
sudo dnf install -y libgccjit-devel






--------------------------------------------------

sudo dnf builddep -y emacs

--------------------------------------------------
# 1. Clear any old configuration files
make distclean
./autogen.sh

# 2. Add the --with-pgtk flag (This replaces traditional X11 rendering)
./configure --with-pgtk --with-native-compilation=aot --with-tree-sitter --with-modules

# 3. Recompile and install
make -j$(nproc)
sudo make install
---------------------------------------------------
sudo dnf install @development-tools @c-development
