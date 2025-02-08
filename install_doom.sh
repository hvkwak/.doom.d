#!/bin/bash

git clone git://git.savannah.gnu.org/emacs.git
cd ~/emacs
git checkout emacs-28 # installs emacs 28.
git pull

# sudo apt update
sudo apt install -y autoconf make texinfo libgtk-3-dev libxpm-dev \
     libjpeg-dev libgif-dev libtiff5-dev libgnutls28-dev libncurses5-dev \
     libjansson-dev libharfbuzz-dev libharfbuzz-bin imagemagick libmagickwand-dev libgccjit-11-dev libgccjit0 libjansson4 libjansson-dev xaw3dg-dev texinfo libx11-dev

export CC="gcc-11"

./autogen.sh
./configure --with-x-toolkit=gtk3 --with-native-compilation --with-json --with-modules --with-harfbuzz --with-compress-install --with-threads --with-included-regex --with-zlib --with-jpeg --with-png --with-imagemagick --with-tiff --with-xpm --with-gnutls --with-xft --with-xml2 --with-mailutils

make -j 8
sudo make install

# installs doom emacs
cd ..
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install


