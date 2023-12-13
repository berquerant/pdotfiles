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

hurl() {
    op="$1"
    if [ -z "$1" ] ; then
        cat - <<EOS
Usage: hurl OP [CURL_OPTS]
OP:
  s, status:
    http code only

  r, response:
    response headers

  v, verbose:
    headers

  j, json:
    info as a json

  h, hjson:
    response headers as a json
EOS
        return 1
    fi

    shift
    case "$op" in
        "s" | "status")
            curl -s -o /dev/null -w "%{http_code}" "$@"
            ;;
        "r" | "response")
            curl -D - -s -o /dev/null "$@"
            ;;
        "v" | "verbose")
            curl -v -s -o /dev/null "$@"
            ;;
        "j" | "json")
            curl -s -o /dev/null -w '%{json}' "$@"
            ;;
        "h" | "hjson")
            curl -s -o /dev/null -w '%{header_json}' "$@"
            ;;
        *)
            return 1
            ;;
    esac
}
