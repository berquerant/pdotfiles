#!/bin/bash

d=$(cd $(dirname $0); pwd)
json2dot="json2dot"
makedepdump="makedepdump"

if [ ! -e "$(which $json2dot)" ] ; then
    "${d}/install-via-git.sh" "$json2dot" || exit 1
fi
if [ ! -e "$(which $makedepdump)" ] ; then
    "${d}/install-via-git.sh" "$makedepdump" || exit 1
fi

# incomplete parsing but sufficient for this project
as_json2dot() {
    awk -F":" '{split($2,xs," ");for(k in xs)printf("{\"src\":{\"id\":\"%s\"},\"dst\":{\"id\":\"%s\"}}\n",$1,xs[k])}'
}

tmpfile="$(mktemp).svg"
"$makedepdump" | as_json2dot | json2dot -o "$tmpfile" && open "$tmpfile"
