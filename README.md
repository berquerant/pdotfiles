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

## microdotfiles

For minimal setup, use [microdotfiles](https://github.com/berquerant/microdotfiles).

## Uninstall

``` shell
./uninstall ruby
```

# Tasks

## init

Install tools using homebrew.

``` shell
git submodule update --init
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

``` shell
if ! grep -q 'EDITOR_EMACS' "${HOME}/.zprofile" ; then
  echo 'export EDITOR_EMACS=lmacs' >> "${HOME}/.zprofile"
fi
DRY=$DRYRUN make -C sh-minimal-init
bin/install-dotfiles.sh $DRYRUN
```

## clean

Uninstall dotfiles configurations.

Inputs: DRYRUN

``` shell
DRY=$DRYRUN make -C sh-minimal-init uninstall
bin/clean-dotfiles.sh $DRYRUN
```

## gitconfig

Generate .gitconfig.

``` shell
make -C sh-minimal-init gitconfig
```

## git

Install git configurations.

Requires: gitconfig, emacs-light

## emacs

Install Emacs.

Requires: fonts, emacs-light, emacs-gui, emacs-cui

## fonts

Install fonts.

``` shell
bin/install-fonts.sh
```

## emacs-light

Install Emacs with minimal configurations.

``` shell
ln -snvf "${DOTFILES_ROOT}/bin/emacs-light.sh" /usr/local/bin/lmacs
ln -snvf "${DOTFILES_ROOT}/bin/emacs-less.sh" /usr/local/bin/umacs
```

## emacs-gui

Install GUI Emacs.

``` shell
./bin/install-emacs.sh
./install emacs-gui
```

## emacs-cui

Install CUI Emacs.

``` shell
EMACSD="$CMACSD" ./bin/install-emacs.sh
./install emacs-cui
```

## update-emacs

Update Emacs.

Requires: update-emacs-gui, update-emacs-cui

## update-emacs-gui

Update GUI Emacs.

``` shell
./install emacs-gui --update
```

## update-emacs-cui

Update CUI Emacs.

``` shell
./install emacs-cui --update
```

## install

Install tools except Emacs.

Requires: install-via-git, sub, util

## update

Update tools except Emacs.

Requires: update-submodules, install-via-git, update-brew, update-sub, update-util

## retry

Retry to install tools except Emacs.

Requires: install-via-git, retry-sub, retry-util

## cycle

Requires: git, install-via-git, update-brew, sub, util

## recycle

Requires: git, install-via-git, update-brew, retry-sub, retry-util

## requirements

Install all [requirements](requirements).

``` shell
bin/install-requirements.sh all
```

## update-submodules

Update git submodules.

``` shell
git submodule update --remote
```

## update-brew

Update brew and formulae.

Env: INSTALL_BREW_NO_INIT=1

``` shell
bin/install-brew.sh
```

## renovate.json

Generate [renovate.json](renovate.json) from [renovate.yml](renovate.yml).

``` shell
yq -o json renovate.yml > renovate.json
```

## ivg-gen

Generate [renovate.lock](ivg/renovate.lock).

``` shell
bin/renovate-ivg.sh gen
```

## ivg-lock

Apply [renovate.lock](ivg/renovate.lock).

``` shell
bin/renovate-ivg.sh lock
```

## emacs-renovate-gen

After `straight-freeze-versions`, generate [renovate.lock](.emacs.d/renovate.lock).

``` shell
bin/emacs-renovate.sh gen
```

## emacs-straight-lock

Apply [renovate.lock](.emacs.d/renovate.lock) to [straight-default.el](.emacs.d/straight-default.el).

``` shell
bin/emacs-renovate.sh lock -c
```

then, `straight-thaw-versions`.

## emacs-clean

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
if which rustup > /dev/null ; then
  rustup self update
else
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
fi
./install r rust
```

## uninstall-rust

Uninstall rust.

``` shell
rustup self uninstall -y
```

## sub

Install sub dependencies.

Requires: rust

``` shell
bin/install-target-bulk.sh < targets/sub
```

## update-sub

Install sub dependencies.

Requires: rust, ivg-lock

``` shell
# bin/install-target-bulk.sh --update < targets/sub
bin/install-target-bulk.sh --retry < targets/sub
```

## retry-sub

Install sub dependencies.

Requires: rust

``` shell
bin/install-target-bulk.sh --retry < targets/sub
```

## util

Install util dependencies.

Requires: util-tools

``` shell
bin/install-via-git-bulk.sh < targets/util
```

## update-util

Install util dependencies.

Requires: util-tools, ivg-lock

``` shell
# bin/install-via-git-bulk.sh --update < targets/util
bin/install-via-git-bulk.sh --retry < targets/util
```

## retry-util

Install util dependencies.

Requires: util-tools

``` shell
bin/install-via-git-bulk.sh --retry < targets/util
```

## util-tools

Requires: awkfmt, textlinter, golangci-lint, uv, ip2bin, semv, emacs-straight-renovate, rnv, json2dot

## json2dot

Install [json2dot](https://github.com/berquerant/json2dot).

``` shell
pip install git+https://github.com/berquerant/json2dot.git@${JSON2DOT_VERSION}
```

## rnv

Install [rnv](https://github.com/berquerant/rnv).

``` shell
cargo install --git https://github.com/berquerant/rnv --tag ${RNV_VERSION}
```

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

## uv

Install [uv](https://github.com/astral-sh/uv).

``` shell
curl -LsSf https://astral.sh/uv/${UV_VERSION}/install.sh | sh
```

## golangci-lint

Verify [golangci-lint](https://github.com/golangci/golangci-lint).

``` shell
golangci-lint config verify -v
ln -snvf "${DOTFILES_ROOT}/bin/flycheck-golangci-lint.sh" /usr/local/bin/flycheck-golangci-lint.sh
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

``` shell
bin/install-via-git-bulk.sh < targets/other
```

## update-other

Install other dependencies.

``` shell
bin/install-via-git-bulk.sh --update < targets/other
```

## retry-other

Install other dependencies.

``` shell
bin/install-via-git-bulk.sh --retry < targets/other
```

## additional

Install additional dependencies.

Requires: additional-tools

``` shell
bin/install-via-git-bulk.sh < targets/additional
```

## update-additional

Install additional dependencies.

Requires: additional-tools

``` shell
bin/install-via-git-bulk.sh --update < targets/additional
```

## retry-additional

Install additional dependencies.

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
