#!/bin/sh

dir=`dirname "$0"`
pwd=`cd "$dir" > /dev/null 2>&1 && pwd`

sudo pacman -Syu
sudo pacman -S --noconfirm yaourt base-devel
sudo pacman -S --noconfirm emacs xmonad xmonad-contrib zsh git curl feh dmenu
sudo pacman -S --noconfirm openssh the_silver_searcher rxvt-unicode unzip xcompmgr

# Install Spacemacs
git clone https://github.com/syl20bnr/spacemacs -b develop ~/.emacs

chsh -s /usr/bin/zsh
# sudo pacman -S texlive-most texlive-langcyrillic
# yaourt -S pplatex-git
