UTIL_IVG_LIST := util/ivg.list
IVG_BULK := bin/install-via-git-bulk.sh

.PHONY: util
util: ## install util dependencies
	cat $(UTIL_IVG_LIST) | $(IVG_BULK)

util-update: ## update util dependencies
	cat $(UTIL_IVG_LIST) | $(IVG_BULK) --update

util-retry: ## retry to install util dependencies
	cat $(UTIL_IVG_LIST) | $(IVG_BULK) --retry

gdbinit: ## install gdbinit for python
	wget -P ~ https://git.io/.gdbinit

trans: ## install https://github.com/soimort/translate-shell
	mkdir -p $(PJTMP) && cd $(PJTMP) && wget git.io/trans && chmod +x ./trans
	ln -snvf $(PROJECT)/tmp/trans /usr/local/bin/trans

devtoys-cli: ## https://devtoys.app/
	rm -rf $(PJTMP)/devtoys-cli/
	mkdir -p $(PJTMP)/devtoys-cli/ && cd $(PJTMP)/devtoys-cli && wget -O devtoys-cli.zip https://github.com/DevToys-app/DevToys/releases/download/v2.0.3.0/devtoys.cli_osx_arm64.zip && unzip devtoys-cli.zip
	ln -snvf $(PJTMP)/devtoys-cli/DevToys.CLI /usr/local/bin/devtoys-cli
