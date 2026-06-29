#!/bin/bash

# install emacs and tools
sudo pacman -S adobe-source-code-pro-fonts \
               bash-completion \
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
               man-pages \ bat \
               openssh \
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
