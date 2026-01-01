#!/bin/bash

d="$(cd "$(dirname "$0")"/.. || exit; pwd)"
. "${d}/bin/common.sh"

message() {
    cecho green "$1"
}

is_dry=false
if [ -n "$1" ]
then
    is_dry=true
    message "Do dry run"
fi

exec_or_dry() {
    run_or_dry "$1" ${is_dry}
}

message "Make symbolic links to this repository root on home directory"

exec_query "Are you sure?" "Bye!"

exec_or_dry "ln -snvf ${d} ~/dotfiles"

exec_or_dry "ln -snvf ${d}/.zshrc ~/.zshrc2"

dotfiles=(
    .pythonrc.py
    .golangci.yml
    .aspell.conf
    .npmrc
    .Brewfile
)

message "Install dotfiles on home directory"

for df in "${dotfiles[@]}"
do
    exec_or_dry "ln -snvf ${d}/${df} ~/"
done

"${d}/bin/install-emacs.sh" "$@"
EMACSD="$CMACSD" "${d}/bin/install-emacs.sh" "$@"

message "Install others"

exec_or_dry "ln -snvf $d/.ruff.toml '${HOME}/Library/Application Support/.ruff.toml'"
exec_or_dry "ln -snvf $d/.subversion '${HOME}/.subversion/config'"

message "Dotfiles installed!"
message "Please make gitconfig to install .gitconfig"
if ${is_dry}
then
    message "Dry run done."
fi

exec_or_dry "sudo chown -R \"$(whoami)\" /usr/local/bin"
