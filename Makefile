help: usage

include sub/Makefile
include util/Makefile

usage: ## print this help
	@cat $(MAKEFILE_LIST) | bin/help-makefile.sh

init: ## install tools using homebrew
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
	@bin/gen-gitconfig.sh
	@echo Please cp $(PJTMP)/.gitconfig ~/ to install.

gitatttibutes: ## generate .gitattributes
	@bin/install-gitattributes.sh

git: gitconfig gitattributes ## install git configurations

emacs: ## install or update emacs
	@bin/install-emacs.sh

update: brew-update pyenv-update node-update rust-update go-update zig-update ## update except emacs
