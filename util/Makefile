gdbinit: ## install gdbinit for python
	wget -P ~ https://git.io/.gdbinit

trans: ## install https://github.com/soimort/translate-shell
	mkdir -p $(PROJECT)/tmp
	cd $(PROJECT)/tmp &&\
	wget git.io/trans &&\
	chmod +x ./trans
	ln -s $(PROJECT)/tmp/trans /usr/local/bin/trans

clean-trans: ## uninstall translate-shell
	rm -f /usr/local/bin/trans $(PROJECT)/tmp/trans

plantuml:
	brew install plantuml
