gdbinit: ## install gdbinit for python
	wget -P ~ https://git.io/.gdbinit

wireshark: ## install wireshark
	brew install wireshark
	brew install --cask wireshark

clean-wireshark: ## uninstall wireshark
	brew uninstall --cask wireshark
	brew uninstall wireshark

get-alerter: alerter ## install alerter

alerter: ## install alerter
	cd $(PROJECT)/tmp && \
	wget https://github.com/vjeantet/alerter/releases/download/004/alerter_v004_darwin_amd64.zip && \
	unzip alerter_v004_darwin_amd64.zip && \
	mv alerter /usr/local/bin && \
	rm -f alerter_v004_darwin_amd64.zip

clean-alerter: ## uninstall alerter
	rm -f /usr/local/bin/alerter

mpv: ## install mpv
	brew install mpv
	mkdir -p $(HOME)/.config $(YTDL_ROOT) $(YTDL_OUTPUTD) $(YTDL_CACHED) $(YTDL_LOGD) $(MPV_SCREENSHOTD) $(MPV_PLAYLISTD) $(MPV_WATCH_LATERD)
	envsubst < $(PROJECT)/etc/mpv/mpv.conf.tpl > $(PROJECT)/etc/mpv/mpv.conf
	ln -snvf $(PROJECT)/etc/mpv $(HOME)/.config/

clean-mpv: ## uninstall mpv
	rm -f $(HOME)/.config/mpv
	brew uninstall mpv

pytools: ## install utilities
	pip install git+https://github.com/berquerant/pytools

clean-pytools: ## uninstall utilities
	pip uninstall pytools

trans: ## install https://github.com/soimort/translate-shell
	mkdir -p $(PROJECT)/tmp
	cd $(PROJECT)/tmp &&\
	wget git.io/trans &&\
	chmod +x ./trans
	ln -s $(PROJECT)/tmp/trans /usr/local/bin/trans

clean-trans: ## uninstall translate-shell
	rm -f /usr/local/bin/trans $(PROJECT)/tmp/trans
