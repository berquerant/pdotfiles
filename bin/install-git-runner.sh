#!/bin/bash

# install-git-runner.sh provides that `ig_run()` to install some tools via git.
#
# Usage:
#
# 1. Prepare shell to be executed to install tools.
# 2. Load this in the shell, like:
#    . install-git-runner.sh
# 3. Write functions to setup, install and rollback.
# 4. Call `ig_run()`.
#
# Environment variables:
#
#   IG_WORKD:
#     working directory.
#     repo will be cloned into $IG_WORKD/reponame
#     reponame is the 2nd argument of `ig_run()`.
#     Default is $PJTMP (also envvar).
#
#   IG_FORCE_UPDATE:
#     If 0, cancel installation when `git pull` does not update repo.
#     Default is 0.
#
# e.g.
#
# setup() {
# ...
#
# install() {
# ...
#
# rollback() {
# ...
#
# ig_run "https://github.com/USERNAME/path/to/repo.git" \
#        "reponame" \
#        "master" \   # branch
#        "setup" \    # refer setup()
#        "install" \  # refer install()
#        "rollback"   # refer rollback()
#
# then
# 1. git clone https://github.com/USERNAME/path/to/repo.git $IG_WORKD/reponame
# 2. git pull
# 3. setup()
# 4. install()
#
# rollback repo and rollback() if errors are occurred.
#
# Requirements:
#   git
#   common.sh

. "${DOTFILES_ROOT}/bin/common.sh"

ig_workd="${IG_WORKD:-$PJTMP}"
ig_force_update="${IG_FORCE_UPDATE:-0}"

ig_run() {
    __ig_repo="$1"
    __ig_reponame="$2"
    __ig_branch="$3"
    __ig_setup_cmd="$4"
    __ig_install_cmd="$5"
    __ig_rollback_cmd="$6"

    __ig_status_up_to_date=10

    cecho green "Run install-git-runner.sh>run with"
    cecho green "  repo: ${__ig_repo}"
    cecho green "  reponame: ${__ig_reponame}"
    cecho green "  branch: ${__ig_branch}"
    cecho green "  setup_cmd: ${__ig_setup_cmd}"
    cecho green "  install_cmd: ${__ig_install_cmd}"
    cecho green "  rollback_cmd: ${__ig_rollback_cmd}"
    cecho green "  working directory: ${ig_workd}"

    __ig_targetd="${ig_workd}/${__ig_reponame}"

    __ig_current_hash="none"
    __ig_next_hash="none"

    __ig_prepare_repo() {
        cecho green "Prepare ${__ig_repo} into ${__ig_targetd}"
        ensure_cd "$ig_workd"
        __ig_cloned=0
        if [ ! -d "$__ig_targetd" ]; then
            __ig_cloned=1
            cecho green "Download ${__ig_repo}"
            git clone "$__ig_repo" "$__ig_targetd" || return $?
        fi

        cd "$__ig_targetd" || return $?
        __ig_current_hash="$(git rev-parse HEAD)"
        cecho green "Now ${__ig_repo} is ${__ig_current_hash}"
        git pull origin "$__ig_branch"|| return $?

        __ig_next_hash="$(git rev-parse HEAD)"
        if [ $__ig_cloned -eq 0 ]; then
            if [ "$__ig_current_hash" = "$__ig_next_hash" ]; then
                if [ $ig_force_update -eq 0 ]; then
                    cecho green "${__ig_repo} is up to date."
                    return $__ig_status_up_to_date
                fi
            fi
        fi
        cecho green "Next ${__ig_repo} will be ${__ig_next_hash}"
    }

    __ig_setup_internal() {
        cecho green "Start setup ${__ig_repo}"
        __ig_prepare_repo && "$__ig_setup_cmd" &&\
            cecho green "End setup ${__ig_repo}"
    }

    __ig_setup() {
        __ig_setup_internal
        ret=$?
        if [ $ret -ne 0 ];  then
            if [ $ret -ne $__ig_status_up_to_date ]; then
                cecho red "Setup ${__ig_repo} failed!"
            else
                cecho green "End setup ${__ig_repo} noop."
            fi
        fi
        return $ret
    }

    __ig_rollback_repo() {
        cd "$__ig_targetd" && git checkout "$__ig_current_hash" &&\
            cecho yellow "Rolled back ${__ig_repo} to ${__ig_current_hash}"
    }

    __ig_rollback_internal() {
        cecho yellow "Start rollback ${__ig_repo}"
        __ig_rollback_repo && "$__ig_rollback_cmd" &&\
            cecho yellow "End rollback ${__ig_repo}"
    }

    __ig_rollback() {
        __ig_rollback_internal
        ret=$?
        if [ $ret -ne 0 ]; then
            cecho red "Rollback ${__ig_repo} failed!"
        fi
        return $ret
    }

    __ig_install() {
        cecho green "Start install ${__ig_repo}"
        "$__ig_install_cmd" &&\
            cecho green "End install ${__ig_repo}"
    }

    __ig_on_success() {
        cecho green "${__ig_reponame} successfully installed!"
        cecho green "  repo: ${__ig_repo}"
        cecho green "  old: ${__ig_current_hash}"
        cecho green "  new: ${__ig_next_hash}"
    }

    __ig_on_failure() {
        ret=$?
        if [ $ret -eq $__ig_status_up_to_date ]; then
            return 0
        fi
        cecho red "Errors were encountered!"
        cecho red "Please check commands and ${__ig_targetd}"
        __ig_rollback
    }

    __ig_setup &&__ig_install && __ig_on_success || __ig_on_failure
}
