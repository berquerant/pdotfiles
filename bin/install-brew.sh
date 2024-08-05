#!/bin/bash

set -e

d=$(cd $(dirname $0)/..; pwd)
. "${d}/bin/common.sh"

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
    __arch="$(uname -m)"
    if [ "$__arch" = "arm64" ]; then
        export PATH="/opt/homebrew/sbin:/opt/homebrew/bin:$PATH"
        echo 'export PATH="/opt/homebrew/sbin:/opt/homebrew/bin:$PATH"' >> ~/.zprofile
    elif [ "$__arch" = "x86_64" ]; then
        export PATH="$PATH:/usr/local/bin"
    fi
else
    message "Homebrew found"
fi

set +e
brew doctor
set -e
brew update
brew upgrade

message "Install tools"
if [ -z "$INSTALL_BREW_NO_INIT" ] ; then
    brew bundle --file "${d}/.Brewfile.init" --no-lock
fi

export HOMEBREW_BUNDLE_FILE_GLOBAL="${d}/.Brewfile"
brew bundle --global --no-lock

if ! which gettext > /dev/null
then
    brew link --force gettext
else
    message "Already gettext linked"
fi

brew cleanup -s

if [ ! -L /Library/Java/JavaVirtualMachines/openjdk.jdk ] ; then
    message "[openjdk] For the system Java wrappers to find this JDK, symlink it"
    sudo ln -sfn /opt/homebrew/opt/openjdk/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk.jdk
fi

message "Homebrew tools installed!"

message "Change shell"

if ! grep -q -e "bash$" /etc/shells
then
    sudo bash -c 'echo /bin/bash >> /etc/shells'
fi
if ! grep -q -e "zsh$" /etc/shells
then
    sudo bash -c 'echo /bin/zsh >> /etc/shells'
fi
if [ "$SHELL" != "/bin/zsh" ] ; then
    chsh -s /bin/zsh
fi

message "You should activate direnv and zsh"
