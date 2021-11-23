# hardware acceleration
hwdec=no
vo=libmpv
correct-pts=yes
sub-visibility=no
screenshot-format=jpg
screenshot-template="%f-%P"
# disable default due to input.conf
input-default-bindings=no
volume=25
cache-on-disk=yes
# options for youtube-dl
ytdl-raw-options=output="${YTDL_OUTPUT_TEMPLATE}"
screenshot-directory="${MPV_SCREENSHOTD}"
cache-dir="${YTDL_CACHED}"
watch-later-directory="${MPV_WATCH_LATERD}"
log-file="${MPV_LOG}"
