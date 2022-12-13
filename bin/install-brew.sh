#!/bin/bash

d=$(cd $(dirname $0); pwd)
. $d/common.sh

message() {
    cecho green "$1"
}

message "Install Xcode"

xcode-select --install

if ! which brew > /dev/null
then
    message "Install Homebrew"
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
else
    message "Homebrew found"
fi

brew doctor
brew update
brew upgrade

message "Install tools"

formulas=(
    bash
    git
    direnv # .envrc
    readline # .pythonrc
    gettext
    peco
    zsh
    zplug
    tmux
    ripgrep
    exa
    bat
    procs
    hexyl
    fd
    tokei
    jq
    yq
    htop
    nkf
    ghq
    openssl
    zlib
    hub
    watch
    aspell
    unar
    nmap
    shellcheck # flymake
    autoconf
    automake
    gcc
    cmake
    libvterm
    libtool
)

for formula in "${formulas[@]}"
do
    brew install $formula
    brew upgrade $formula
done

if ! which gettext > /dev/null
then
    brew link --force gettext
else
    message "Already gettext linked" green
fi

message "Install GNU tools, g-prefixed commands and others"

gnu_formulas=(
    coreutils
    diffutils
    findutils
    binutils
    gawk
    gnu-sed
    gnu-tar
    grep
    gzip
    pandoc
    wget
    unzip
    gnu-time
)

for formula in "${gnu_formulas[@]}"
do
    brew install $formula
    brew upgrade $formula
done

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
