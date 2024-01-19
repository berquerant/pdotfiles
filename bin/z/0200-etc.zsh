MPV_SETTINGS_ZSH="${DOTFILES_ROOT}/ivg/repos/mpv-settings/mpv.zsh"

__mpv_installed() {
    type mpv >/dev/null 2>&1 && [ -f "$MPV_SETTINGS_ZSH" ]
}

load_mpv() {
    source "$MPV_SETTINGS_ZSH"
}

if __mpv_installed ; then
    load_mpv
fi
