gdbinit: ## install gdbinit for python
	wget -P ~ https://git.io/.gdbinit

trans: ## install https://github.com/soimort/translate-shell
	mkdir -p $(PJTMP) && cd $(PJTMP) && wget git.io/trans && chmod +x ./trans
	ln -snvf $(PROJECT)/tmp/trans /usr/local/bin/trans

openai-chat-web: python ## install openai-chat-web
	bin/install-via-git.sh openai-chat-web

openai-chat-web-update: ## update openai-chat-web
	bin/install-via-git.sh openai-chat-web --update

gbrowse: ## install gbrowse
	bin/install-via-git.sh gbrowse

gbrowse-update: ## update gbrowse
	bin/install-via-git.sh gbrowse --update
