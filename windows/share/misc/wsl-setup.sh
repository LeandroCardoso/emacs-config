#!/bin/bash

# install emacs and tools
sudo pacman -S 7zip \
               adobe-source-code-pro-fonts \
               bat \
               clang \
               clang-tools-extra \
               codespell \
               cppcheck \
               diffutils \
               emacs-wayland \
               fish \
               fzf \
               global \
               grep \
               hunspell \
               hunspell-en_us \
               keyd \
               man-pages \
               noto-fonts \
               noto-fonts-emoji \
               openssh \
               ripgrep \
               tldr \
               ttf-nerd-fonts-symbols \
               ttf-nerd-fonts-symbols-mono \
               unzip \
               zip

sudo pacman -S --asdeps less mandoc pkgfile python wl-clipboard xdg-utils

# locale
sudo localectl set-keymap us-acentos
sudo localectl set-x11-keymap "us" "pc105" "intl" "lv3:ralt_alt"

gsettings set org.gnome.desktop.wm.preferences button-layout 'appmenu:minimize,maximize,close'
