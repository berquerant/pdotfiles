#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-via-git.sh"

export IVG_WORKD="$PJTMP"

readonly attr_reponame="gitattributes"
readonly attr_repod="${IVG_WORKD}/${attr_reponame}"

readonly attr_location="${HOME}/.gitattributes"
readonly attr_location_backup="${HOME}/.gitattributes.bk"

backup_attr() {
    mv -f "$attr_location" "$attr_location_backup"
}

restore_attr() {
    mv -f "$attr_location_backup" "$attr_location"
}

setup_attr() {
    backup_attr || brew install ripgrep
}

rollback_attr() {
    restore_attr
}

install_attr() {
    cd "$attr_repod" || return $?
    readonly current_hash="$(git rev-parse HEAD)"
    readonly targets=(
        "C++"
        "Common"
        "Go"
        "Java"
        "Markdown"
        "Perl"
        "Python"
        "Rails"
        "Rust"
        "sql"
        "Web"
    )
    echo "# From ${current_hash}" > "$attr_location"
    for target in "${targets[@]}" ; do
        local t="${target}.gitattributes"
        cecho green "Load ${t}..."
        rg -v "^(#|$)" < "${attr_repod}/${t}" >> "$attr_location" || return $?
    done
}

ivg_run "https://github.com/alexkaratarakis/gitattributes.git" \
        "$attr_reponame" \
        "master" \
        "setup_attr" \
        "install_attr" \
        "rollback_attr"
