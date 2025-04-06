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
Requires: trans, awkfmt, textlinter, golangci-lint

``` shell
bin/install-via-git-bulk.sh < targets/util
```

## update-util

Install util dependencies.

interactive: true
Requires: trans, awkfmt, textlinter, golangci-lint

``` shell
bin/install-via-git-bulk.sh --update < targets/util
```

## retry-util

Install util dependencies.

interactive: true
Requires: trans, awkfmt, textlinter, golangci-lint

``` shell
bin/install-via-git-bulk.sh --retry < targets/util
```

## golangci-lint

Install [golangci-lint](https://github.com/golangci/golangci-lint).

``` shell
set -e
mkdir -p "${PJTMP}"
cd "${PJTMP}"
wget -O "golangci-lint.tar.gz" "https://github.com/golangci/golangci-lint/releases/download/v1.64.8/golangci-lint-1.64.8-darwin-arm64.tar.gz"
tar xvzf "golangci-lint.tar.gz"
ln -snvf "${PJTMP}/golangci-lint-1.64.8-darwin-arm64/golangci-lint" /usr/local/bin/golangci-lint
```

## textlinter

Install [textlint.sh](bin/textlint.sh).

``` shell
ln -snvf "${DOTFILES_ROOT}/bin/textlint.sh" /usr/local/bin/textlinter
```

## trans

Install `https://github.com/soimort/translate-shell`.

``` shell
mkdir -p "$PJTMP" && cd "$PJTMP" && wget git.io/trans && chmod +x ./trans
ln -snvf "${PJTMP}/trans" /usr/local/bin/trans
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

## gdbinit

Install gdbinit for Python.

``` shell
wget -P ~ https://git.io/.gdbinit
```

## devtoys-cli

Install devtoys CLI.

``` shell
dest="${PJTMP}/devtoys-cli"
rm -rf "$dest"
mkdir -p "$dest" && cd "$dest" && wget -O devtoys-cli.zip https://github.com/DevToys-app/DevToys/releases/download/v2.0.3.0/devtoys.cli_osx_arm64.zip && unzip devtoys-cli.zip
ln -snvf "${dest}/DevToys.CLI" /usr/local/bin/devtoys-cli
```

## additional

Install additional dependencies.

interactive: true

``` shell
bin/install-via-git-bulk.sh < targets/additional
```

## update-additional

Install additional dependencies.

interactive: true

``` shell
bin/install-via-git-bulk.sh --update < targets/additional
```

## retry-additional

Install additional dependencies.

interactive: true

``` shell
bin/install-via-git-bulk.sh --retry < targets/additional
```
