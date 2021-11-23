#!/bin/zsh

source $DOTFILES_ROOT/bin/common.sh

# https://mpv.io/manual/master/
# https://blog.ishotihadus.com/archives/307
# https://poor-user.blogspot.com/2017/10/mpv-player-mpvconf-2.html
# https://masayoshi-9a7ee.hatenablog.com/entry/20150905/1441414821
export YTDL_ROOT=$HOME/ytdl
export YTDL_OUTPUTD="$YTDL_ROOT/download/output"
mkdir -p $YTDL_OUTPUTD
export YTDL_OUTPUT_TEMPLATE="$YTDL_OUTPUTD/%(title)s-%(id)s.%(ext)s"
export YTDL_CACHED="$YTDL_ROOT/cache"
mkdir -p $YTDL_CACHED
export YTDL_LOGD="$YTDL_ROOT/log"
mkdir -p $YTDL_LOGD
export MPV_SCREENSHOTD="$YTDL_ROOT/download/screenshot"
mkdir -p $MPV_SCREENSHOTD
export MPV_PLAYLISTD="$YTDL_ROOT/playlist"
mkdir -p $MPV_PLAYLISTD
export MPV_WATCH_LATERD="$YTDL_ROOT/watch_later"
mkdir -p $MPV_WATCH_LATERD
export MPV_LOG="$YTDL_LOGD/mpv.log"
export MPV_MUSIC_WINDOW="mpv_music"
export MPV_VIDEO_WINDOW="mpv_video"
export MUSIC_ROOT="$HOME/Music/iTunes/iTunes Media/Music"
export MPV_COMMON_OPTIONS="--msg-level=all=status"
# choose playlist
alias mpv-playlist='find $MPV_PLAYLISTD -type f | sort -r | peco'
alias dump-playlist='cat `mpv-playlist`'
alias find-music='find $MUSIC_ROOT -type f | peco'
alias mpv-normal='mpv $MPV_COMMON_OPTIONS'
mpv-gen-playlist-name() {
    local now=`date "+%Y-%m-%d_%H:%M:%S"`
    local uid=`uuidgen`
    echo "${now}_${uid}"
}
# select music, play and save them
mpv-find-and-play() {
    local name=`mpv-gen-playlist-name`
    local pfile="$MPV_PLAYLISTD/$name.m3u"
    find $MUSIC_ROOT -type f | peco > $pfile
    local opt="$MPV_COMMON_OPTIONS --no-video --ytdl-format='worstvideo+bestaudio' --shuffle --playlist=$pfile"
    local cmd="mpv $opt"
    tmux new-window -n $MPV_MUSIC_WINDOW $cmd
    cecho green "Playing $pfile"
}
# construct new playlist interactively
new-playlist() {
    local name
    if [ $# = 0 ]
    then
        name=`mpv-gen-playlist-name`
    else
        name=$1
    fi
    local pfile="$MPV_PLAYLISTD/$name.m3u"
    if [ -e "$pfile" ]
    then
        local c
        cecho yellow "$pfile already exists"
        read -k "c?Overwrite(w) Append(a) Exit(e) > "
        echo
        case "$c" in
            "w") rm -f $pfile ;;
            "a") ;;
            "e") return 0 ;;
            *) return 1 ;;
        esac
    else
        cecho green "New playlist: $pfile"
        cecho green "OK? (y/n)"
        if ! read -q ; then return 1 ; fi
        echo
    fi
    while true
    do
        find $MUSIC_ROOT -type f | peco >> $pfile
        cecho green "Continue? (y/n)"
        if ! read -q
        then
            echo
            cecho green "Playlist: $pfile"
            cat $pfile
            return 0
        fi
        echo
    done
}
# play music
# mpv-music
#   select playlist and play it
# mpv-music stdin
#   accept music names from stdin
mpv-music() {
    local opt="$MPV_COMMON_OPTIONS --no-video --ytdl-format='worstvideo+bestaudio'"
    local cmd
    # return control back to terminal but I do not want to lose stdin
    if [ $# = 0 ]
    then
        local p=`mpv-playlist`
        cmd="mpv $opt --shuffle --playlist=$p"
    else
        if [ $1 = "stdin" ]
        then
            local name=`mpv-gen-playlist-name`
            local pfile="$MPV_PLAYLISTD/$name.m3u"
            cat - > $pfile
            cmd="mpv $opt --playlist=$pfile"
        else
            cmd="mpv $opt $@"
        fi
    fi
    tmux new-window -n $MPV_MUSIC_WINDOW $cmd
}
# play video
# mpv-video [uri]
mpv-video() {
    if [ $# = 0 ]
    then
        local p=`mpv-playlist`
        local cmd="mpv $MPV_COMMON_OPTIONS --ontop --border=no --autofit=600 --geometry=100%:100% --ytdl-format='[height<=480]+bestaudio' --shuffle --playlist=$p"
        tmux new-window -n $MPV_MUSIC_WINDOW $cmd
    else
        local cmd="mpv $MPV_COMMON_OPTIONS --ontop --border=no --autofit=600 --geometry=100%:100% --ytdl-format='[height<=480]+bestaudio' $@"
        tmux new-window -n $MPV_MUSIC_WINDOW $cmd
    fi
}
# quit music or video spawned by mpv-music or mpv-video
mpv-kill() {
    pkill -SIGUSR1 -f mpv
}
ytdl-execute() {
    youtube-dl --output "$YTDL_OUTPUT_TEMPLATE" --cache-dir "$YTDL_CACHED" --write-info-json --write-thumbnail $@
}
