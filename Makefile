help: usage

include sub/Makefile
include util/Makefile
include additional/Makefile

dependencies: ## generate depndency graph
	bin/make-dependencies-graph.sh

usage: ## print this help
	@cat $(MAKEFILE_LIST) | bin/help-makefile.sh

init: ## install tools using homebrew
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

git: gitconfig emacs-light ## install git configurations

fonts: ## install fonts
	@bin/install-fonts.sh

emacs-light: ## install emacs-light.sh
	ln -snvf $(DOTFILES_ROOT)/bin/emacs-light.sh /usr/local/bin/lmacs

emacs: fonts emacs-gui emacs-cui emacs-light ## install emacs

emacs-gui: ## install GUI emacs
	@bin/install-via-git.sh emacs-gui

emacs-cui: ## install CUI emacs
	@bin/install-via-git.sh emacs-cui

update-emacs: update-emacs-cui update-emacs-gui emacs-light

update-emacs-gui: ## update GUI emacs
	@bin/install-via-git.sh emacs-gui --update

update-emacs-cui: ## update CUI emacs
	@bin/install-via-git.sh emacs-cui --update

emacs-check-packages: ## update local package repos
	@bin/emacs-package.sh check

install: install-via-git sub util ## install tools
update: install-via-git brew-update sub-update util-update ## update except emacs
retry: install-via-git sub-retry util-retry ## retry to install tools

.PHONY: requirements
requirements: ## install all requirements
	@bin/install-requirements.sh all

git-global: ## modify global git settings
	@echo you should set user.email and user.name
	git config --global color.ui auto
	git config --global diff.renames true
	git config --global core.attributesfile ~/.gitattributes
	git config --global log.abbrevCommit true
	git config --global core.ignorecase false
	git config --global core.autocrlf false

brew-update: ## update brew packages
	brew update
	brew bundle --global --no-lock
	brew upgrade
	brew cleanup

renovate.json: renovate.yml ## generate renovate.json
	yq -ojson $< > $@

cycle: git install-via-git brew-update sub util emacs-check-packages
recycle: git install-via-git brew-update sub-retry util-retry emacs-check-packages
