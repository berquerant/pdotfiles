#!/bin/zsh

export MPV_SETTINGS_SH="${DOTFILES_ROOT}/ivg/repos/mpv-settings/mpv.sh"

__mpv_installed() {
    type mpv >/dev/null 2>&1 && [ -f "$MPV_SETTINGS_SH" ]
}

load_mpv() {
    . "$MPV_SETTINGS_SH"
}

if __mpv_installed ; then
    load_mpv
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

kill_mdworkers() {
    sudo mdutil -a -i off
}
