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
               fzf \
               global \
               grep \
               hunspell \
               hunspell-en_us \
               keyd \
               openssh \
               ripgrep \
               tldr \
               ttf-nerd-fonts-symbols \
               unzip \
               zip

# locale
sudo localectl set-keymap us-acentos
sudo localectl set-x11-keymap "us" "pc105" "intl" "lv3:ralt_alt"
