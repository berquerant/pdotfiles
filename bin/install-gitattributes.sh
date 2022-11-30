#!/bin/bash

source "${DOTFILES_ROOT}/bin/common.sh"

DEST="${HOME}/.gitattributes"
TARGETS=(
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

cecho green "Install .gitattributes."

cecho green "Backing up .gitattributes."
DEST_BKUP="${DEST}.bk"
mv -f $DEST $DEST_BKUP

mkdir -p "${PROJECT}/tmp"
cd "${PROJECT}/tmp"

if [ ! -d ./gitattributes ]; then
    cecho "Download gitattributes repo."
    git clone https://github.com/alexkaratarakis/gitattributes.git
    git checkout master
fi

cd gitattributes
CURRENT_HASH=$(git rev-parse HEAD)
cecho green "Now gitattributes is ${CURRENT_HASH}"
git pull
NEXT_HASH=$(git rev-parse HEAD)
cecho green "Next gitattributes will be ${NEXT_HASH}"

GITATTRIBUTES="${PROJECT}/tmp/gitattributes"

generate() {
    echo "# From ${NEXT_HASH}" > $DEST
    for target in "${TARGETS[@]}" ; do
        local t="${target}.gitattributes"
        cecho green "Load ${t}..."
        rg -v "^(#|$)" < "${GITATTRIBUTES}/${t}" >> $DEST
    done
}

rollback() {
    mv -f $DEST_BKUP $DEST
    cd $GITATTRIBUTES
    git checkout $CURRENT_HASH
    cecho yellow "gitattributes rolled back!"
}

on_failed() {
    cecho red "Install gitattributes ${NEXT_HASH} failed!"
    rollback
    cecho red "Now gitattributes is ${CURRENT_HASH}"
    exit 1
}

on_success() {
    cecho green ".gitattribtes Installed!"
    cecho green "Now .gitattributes is ${NEXT_HASH}"
    cecho green "Done!"
}


generate && on_success || on_failed
