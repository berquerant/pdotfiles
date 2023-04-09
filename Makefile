help: usage

include sub/Makefile
include util/Makefile

usage: ## print this help
	@cat $(MAKEFILE_LIST) | bin/help-makefile.sh

init: ## init submodules and install tools using homebrew
	@git submodule update --init
	@bin/install-brew.sh

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

emacs: emacs-gui emacs-cui ## install or update emacs

emacs-gui: ## install or update GUI emacs
	@bin/install-emacs-gui.sh

emacs-cui: ## install or update CUI emacs
	@bin/install-emacs-cui.sh

update: brew-update pyenv-update node-update rust-update ruby-update go-update zig-update ## update except emacs
