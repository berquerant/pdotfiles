#!/bin/bash

set -e

guess_en_or_ja() {
    "${DOTFILES_ROOT}/bin/guesslang.sh" en ja --top --default en |\
        jq -r '.[].lang'
}

target="$1"
if [ "$target" = "-h" ] || [ "$target" = "--help" ] ; then
    textlint --help
    exit
fi

if [ -z "$target" ] ; then
    stdin=1
    target="$(mktemp)"
    cat - > "$target"
fi

lang="$(guess_en_or_ja < "$target")"
config="${DOTFILES_ROOT}/textlint/${lang}.yml"

lint() {
    textlint --no-color --config "$config" --format json "$@" |\
        jq -r '.[0] | .filePath as $p | .messages[] | (.message|gsub("\\n";" ")) as $m | "\($p):\(.loc.start.line):\(.loc.start.column):[\(.ruleId)]\($m)"'
}

# unknown extensions are treated as .txt
# https://github.com/textlint/textlint/blob/9e57c912a437ef7c933e996d7b2d68cc2c7fdad6/packages/@textlint/textlint-plugin-markdown/src/MarkdownProcessor.ts#L16
# https://github.com/textlint/textlint/blob/9e57c912a437ef7c933e996d7b2d68cc2c7fdad6/packages/%40textlint/textlint-plugin-text/src/TextProcessor.ts#L17
if echo "$target" | grep -q -v -E '\.(txt|text|md|markdown|mdown|mkdn|mkd|mkdwn|mkdown|ron|mdx)$' ; then
    filename="${target}.txt"
    name="$target"
    if [ -n "$stdin" ] ; then
        name="stdin"
    fi
    lint --stdin --stdin-filename "$filename" < "$target" |\
        sed "s|^${filename}:|${name}:|"
else
    lint "$target"
fi
