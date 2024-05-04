#!/bin/zsh

dman() {
    image=docker-man:debian
    case "$1" in
        "d")
            shift
            ;;
        "u")
            shift
            image=docker-man:ubuntu
            ;;
    esac
    docker run --rm -it "$image" "$@"
}

dcommit() {
    container_id="$(docker ps|awk 'NR>1'|peco|cut -d' ' -f1)"
    read "image_name?name>"
    docker commit "$container_id" "$image_name"
}

dpwd() {
    docker run --rm -v "${PWD}:/usr/src/app" -w "/usr/src/app" "$@"
}
