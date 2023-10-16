util:
	cat util/ivg.list | bin/install-via-git-bulk.sh

util-update:
	cat util/ivg.list | bin/install-via-git-bulk.sh --update

gdbinit: ## install gdbinit for python
	wget -P ~ https://git.io/.gdbinit

trans: ## install https://github.com/soimort/translate-shell
	mkdir -p $(PJTMP) && cd $(PJTMP) && wget git.io/trans && chmod +x ./trans
	ln -snvf $(PROJECT)/tmp/trans /usr/local/bin/trans
