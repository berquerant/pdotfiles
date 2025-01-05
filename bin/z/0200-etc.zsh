#!/bin/zsh

MPV_SETTINGS_SH="${DOTFILES_ROOT}/ivg/repos/mpv-settings/mpv.sh"

__mpv_installed() {
    type mpv >/dev/null 2>&1 && [ -f "$MPV_SETTINGS_SH" ]
}

load_mpv() {
    . "$MPV_SETTINGS_SH"
}

if __mpv_installed ; then
    load_mpv
fi

if which mf > /dev/null 2>&1 && [ -n "$MUSIC_ROOT" ]; then
    export MF_CONFIG="${DOTFILES_ROOT}/.mf.yml"
    export MF_INDEX="${TMPD}/.mf.index.json"
    ffindex() {
        mf --config "${DOTFILES_ROOT}/.mf.yml" -v > "$MF_INDEX"
    }
    ffquery() {
        if [ ! -f "$MF_INDEX" ] ; then
            ffindex
        fi
        mf -i "$MF_INDEX" "$@"
    }
fi

histfile_stat() {
    cat "$HISTFILE" |\
        rg -o ';.+' |\
        tr -d ';' |\
        cut -d " " -f "1-${1:-1}" |\
        sort |\
        uniq -c |\
        sort -nk 1
}
