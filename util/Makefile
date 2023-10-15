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

ip2bin: ## install ip2bin
	bin/install-via-git.sh ip2bin

ip2bin-update: ## update ip2bin
	bin/install-via-git.sh ip2bin --update

cv: ## install cv
	bin/install-via-git.sh cv

cv-update: ## update cv
	bin/install-via-git.sh cv --update

docker-man: ## install docker-man
	bin/install-via-git.sh docker-man

docker-man-update: ## update docker-man
	bin/install-via-git.sh docker-man --update

joiny: ## install joiny
	bin/install-via-git.sh joiny

joiny-update: ## update joiny
	bin/install-via-git.sh joiny --update

csvcut: ## install csvcut
	bin/install-via-git.sh csvcut

csvcut-update: ## update csvcut
	bin/install-via-git.sh csvcut --update

sqlite-csv: ## install sqlite-csv
	bin/install-via-git.sh sqlite-csv

sqlite-csv-update: ## update sqlite-csv
	bin/install-via-git.sh sqlite-csv --update
