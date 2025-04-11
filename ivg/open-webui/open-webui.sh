#!/bin/bash

cd "${DOTFILES_ROOT}/ivg/repos/open-webui" || exit 1
docker compose -f docker-compose-local.yml "$@"
