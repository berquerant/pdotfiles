help: usage

include sub/Makefile
include util/Makefile

usage: ## print this help
	@cat $(MAKEFILE_LIST) | bin/help-makefile.sh

init: ## init submodules and install tools using homebrew
	@bin/install-brew.sh

install-via-git: ## install tool to install
	@bin/install-via-git-go.sh

dry-deploy: ## dry run deploying configurations
	@bin/install-dotfiles.sh dry

deploy: ## deploy configurations
	@bin/install-dotfiles.sh

dry-clean: ## dry run uninstall dotfiles
	@bin/clean-dotfiles.sh dry

clean: ## uninstall dotfiles
	@bin/clean-dotfiles.sh

gitconfig: ## generate .gitconfig
	@bin/install-gitconfig.sh

gitatttibutes: ## generate .gitattributes
	@bin/install-gitattributes.sh

git: gitconfig gitattributes ## install git configurations

emacs: emacs-gui emacs-cui ## install emacs

emacs-gui: ## install GUI emacs
	@bin/install-via-git.sh emacs-gui

emacs-cui: ## install CUI emacs
	@bin/install-via-git.sh emacs-cui

update-emacs: update-emacs-cui update-emacs-gui

update-emacs-gui: ## update GUI emacs
	@bin/install-via-git.sh emacs-gui --update

update-emacs-cui: ## update CUI emacs
	@bin/install-via-git.sh emacs-cui --update

update: brew-update pyenv-update node-update rust-update ruby-update go-update zig-update ## update except emacs
