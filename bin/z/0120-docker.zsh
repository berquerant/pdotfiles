#!/bin/zsh

drun() {
    "${DOTFILES_ROOT}/bin/docker-rmit.sh" "$@"
}

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
    "${DOTFILES_ROOT}/bin/docker-rmit.sh" "$image" "$@"
}

dterraform() {
    "${DOTFILES_ROOT}/bin/docker-rmit.sh" hashicorp/terraform:latest "$@"
}

dcommit() {
    container_id="$(docker ps|awk 'NR>1'|peco|cut -d' ' -f1)"
    read "image_name?name>"
    docker commit "$container_id" "$image_name"
}
