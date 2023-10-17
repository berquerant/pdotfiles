#!/bin/bash

set -e

d=$(cd $(dirname $0); pwd)
. $d/common.sh

message() {
    cecho green "$1"
}

message "Install Xcode"

set +e
xcode-select --install
set -e

if ! which brew > /dev/null
then
    message "Install Homebrew"
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
else
    message "Homebrew found"
fi

set +e
brew doctor
set -e
brew update
brew upgrade

message "Install tools"
brew bundle --file "${d}/../.Brewfile"

if ! which gettext > /dev/null
then
    brew link --force gettext
else
    message "Already gettext linked"
fi

brew cleanup -s

message "Homebrew tools installed!"

message "Change shell"

if ! grep -e "bash$" /etc/shells > /dev/null
then
    sudo bash -c 'echo /usr/local/bin/bash >> /etc/shells'
fi
if ! grep -e "zsh$" /etc/shells > /dev/null
then
    sudo bash -c 'echo /usr/local/bin/zsh >> /etc/shells'
fi
chsh -s /usr/local/bin/zsh

message "You should activate direnv and zsh"
