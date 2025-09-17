# Dotfiles for macOS

```shell
git clone https://github.com/berquerant/pdotfiles "${HOME}/src/github.com/berquerant/pdotfiles"
cd "${HOME}/src/github.com/berquerant/pdotfiles"
./xc init
ZDOTDIR="$PWD" zsh -i
./xc deploy ""
git config --global user.name "$USERNAME"
git config --global user.email "$USEREMAIL"
./xc git
./xc emacs
./xc install
```

## Install one by one

``` shell
./install ruby
```

Install libraries.

``` shell
./install r ruby
```

Install languages and libraries.

``` shell
./install t ruby
```

### targets and requirements

The names listed in the files in [targets](targets) can be specified as [./install](install) argument.

The file names in [requirements](targets) can be specified as [./install r](install) argument.

Lines in target and requirements files can be commented out with leading '#'.

## Uninstall

``` shell
./uninstall ruby
```

# Tasks

## init

Install tools using homebrew.

interactive: true

``` shell
bin/install-brew.sh
```

## install-via-git

Install tools to install by docker.

``` shell
bin/install-via-git-go.sh
```

## deploy

Deploy dotfiles configurations.

Inputs: DRYRUN
interactive: true

``` shell
bin/install-dotfiles.sh $DRYRUN
```

## clean

Uninstall dotfiles configurations.

Inputs: DRYRUN
interactive: true

``` shell
bin/clean-dotfiles.sh $DRYRUN
```

## gitconfig

Generate .gitconfig.

interactive: true

``` shell
bin/install-gitconfig.sh
```

## git

Install git configurations.

interactive: true
Requires: gitconfig, emacs-light

## emacs

Install Emacs.

interactive: true
Requires: fonts, emacs-light, emacs-gui, emacs-cui

## fonts

Install fonts.

interactive: true

``` shell
bin/install-fonts.sh
```

## emacs-light

Install Emacs with minimal configurations.

``` shell
ln -snvf "${DOTFILES_ROOT}/bin/emacs-light.sh" /usr/local/bin/lmacs
```

## emacs-gui

Install GUI Emacs.

interactive: true

``` shell
./bin/install-emacs.sh
./install emacs-gui
```

## emacs-cui

Install CUI Emacs.

interactive: true

``` shell
EMACSD="$CMACSD" ./bin/install-emacs.sh
./install emacs-cui
```

## update-emacs

Update Emacs.

interactive: true
Requires: update-emacs-gui, update-emacs-cui

## update-emacs-gui

Update GUI Emacs.

interactive: true

``` shell
./install emacs-gui --update
```

## update-emacs-cui

Update CUI Emacs.

interactive: true

``` shell
./install emacs-cui --update
```

## emacs-check-packages

Rebuild any packages that have been modified.

``` shell
bin/emacs-package.sh check
```

## install

Install tools except Emacs.

interactive: true
Requires: install-via-git, sub, util

## update

Update tools except Emacs.

interactive: true
Requires: install-via-git, update-brew, update-sub, update-util

## retry

Retry to install tools except Emacs.

interactive: true
Requires: install-via-git, retry-sub, retry-util

## cycle

interactive: true
Requires: git, install-via-git, update-brew, sub, util, emacs-check-packages

## recycle

interactive: true
Requires: git, install-via-git, update-brew, retry-sub, retry-util, emacs-check-packages

## requirements

Install all [requirements](requirements).

interactive: true

``` shell
bin/install-requirements.sh all
```

## update-brew

Update brew and formulae.

interactive: true
Env: INSTALL_BREW_NO_INIT=1

``` shell
bin/install-brew.sh
```

## renovate.json

Generate [renovate.json](renovate.json) from [renovate.yml](renovate.yml).

``` shell
yq -o json renovate.yml > renovate.json
```

## emacs-renovate

Generate [renovate.lock](.emacs.d/renovate.lock).

``` shell
bin/emacs-renovate.sh renovate
```

## emacs-lock

Apply [renovate.lock](.emacs.d/renovate.lock). to [straight-default.el](.emacs.d/straight-default.el).

``` shell
bin/emacs-renovate.sh lock
```

## emacs-clean

interactive: true
Requires: emacs-clean-cache, emacs-clean-straight

## emacs-clean-cache

``` shell
bin/clean-emacs.sh cache
```

## emacs-clean-straight

``` shell
bin/clean-emacs.sh straight
```

## rust

Install rust.

``` shell
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
./install r rust
```

## uninstall-rust

Uninstall rust.

``` shell
rustup self uninstall -y
```

## sub

Install sub dependencies.

interactive: true
Requires: rust

``` shell
bin/install-target-bulk.sh < targets/sub
```

## update-sub

Install sub dependencies.

interactive: true
Requires: rust

``` shell
bin/install-target-bulk.sh --update < targets/sub
```

## retry-sub

Install sub dependencies.

interactive: true
Requires: rust

``` shell
bin/install-target-bulk.sh --retry < targets/sub
```

## util

Install util dependencies.

interactive: true
Requires: util-tools

``` shell
bin/install-via-git-bulk.sh < targets/util
```

## update-util

Install util dependencies.

interactive: true
Requires: util-tools

``` shell
bin/install-via-git-bulk.sh --update < targets/util
```

## retry-util

Install util dependencies.

interactive: true
Requires: util-tools

``` shell
bin/install-via-git-bulk.sh --retry < targets/util
```

## util-tools

Requires: awkfmt, textlinter, golangci-lint, uv, objdiff, ip2bin, semv, emacs-straight-renovate

## emacs-straight-renovate

Install [emacs-straight-renovate](https://github.com/berquerant/emacs-straight-renovate).

``` shell
pip install git+https://github.com/berquerant/emacs-straight-renovate.git@${EMACS_STRAIGHT_RENOVATE_VERSION}
```

## semv

Install [semv](https://github.com/berquerant/semv).

``` shell
cargo install --git https://github.com/berquerant/semv --tag ${SEMV_VERSION}
```

## ip2bin

Install [ip2bin](https://github.com/berquerant/ip2bin-rust).

``` shell
cargo install --git https://github.com/berquerant/ip2bin-rust --tag ${IP2BIN_VERSION}
```

## objdiff

Install [objdiff](https://github.com/berquerant/k8s-object-diff-go).

``` shell
ln -snvf "$(which objdiff)" /usr/local/bin/kd
```

## uv

Install [uv](https://github.com/astral-sh/uv).

``` shell
curl -LsSf https://astral.sh/uv/${UV_VERSION}/install.sh | sh
```

## golangci-lint

Verify [golangci-lint](https://github.com/golangci/golangci-lint).

``` shell
golangci-lint config verify -v
```

## textlinter

Install [textlint.sh](bin/textlint.sh).

``` shell
ln -snvf "${DOTFILES_ROOT}/bin/textlint.sh" /usr/local/bin/textlinter
```

## awkfmt

Install awkfmt.

``` shell
ln -snvf "${DOTFILES_ROOT}/bin/awkfmt.sh" /usr/local/bin/awkfmt
```

## other

Install other dependencies.

interactive: true

``` shell
bin/install-via-git-bulk.sh < targets/other
```

## update-other

Install other dependencies.

interactive: true

``` shell
bin/install-via-git-bulk.sh --update < targets/other
```

## retry-other

Install other dependencies.

interactive: true

``` shell
bin/install-via-git-bulk.sh --retry < targets/other
```

## additional

Install additional dependencies.

interactive: true
Requires: additional-tools

``` shell
bin/install-via-git-bulk.sh < targets/additional
```

## update-additional

Install additional dependencies.

interactive: true
Requires: additional-tools

``` shell
bin/install-via-git-bulk.sh --update < targets/additional
```

## retry-additional

Install additional dependencies.

interactive: true
Requires: additional-tools

``` shell
bin/install-via-git-bulk.sh --retry < targets/additional
```

## additional-tools

Requires: metafind, local-jukebox

## metafind

Install [metafind](https://github.com/berquerant/metafind).

``` shell
go install "github.com/berquerant/metafind/cmd/mf@${METAFIND_VERSION}"
```

## local-jukebox

Install [local-jukebox](https://github.com/berquerant/local-jukebox).

``` shell
go install "github.com/berquerant/local-jukebox/cmd/jukebox@${LOCAL_JUKEBOX_VERSION}"
```
