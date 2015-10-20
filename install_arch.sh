#!/bin/sh

dir=`dirname "$0"`
pwd=`cd "$dir" > /dev/null 2>&1 && pwd`

sudo pacman -Syu
sudo pacman -S --noconfirm yaourt base-devel zsh curl feh dmenu rxvt-unicode # Needed tools
sudo pacman -S --noconfirm xmonad xmonad-contrib xmobar # Window Manager
sudo pacman -S --noconfirm openssh the_silver_searcher unzip emacs adobe-source-code-pro-fonts # Nice to have

# Install Spacemacs
git clone https://github.com/syl20bnr/spacemacs -b develop ~/.emacs.d

chsh -s /usr/bin/zsh
# sudo pacman -S texlive-most texlive-langcyrillic
# yaourt -S pplatex-git
yaourt google-chrome
