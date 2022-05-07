git-global: ## modify global git settings
	@echo you should set user.email and user.name
	git config --global color.ui auto
	git config --global diff.renames true
	git config --global core.attributesfile ~/.gitattributes
	git config --global core.excludesfile ~/.gitignore
	git config --global log.abbrevCommit true
	git config --global core.ignorecase false

get-go: go go-packages golangci-lint ## install go and make it available

go: ## install go
	brew install go

go-packages: ## install golang utilities
	go install golang.org/x/tools/gopls@latest
	go install golang.org/x/tools/cmd/goimports@latest
	go install github.com/go-delve/delve/cmd/dlv@latest
	go install github.com/google/gops@latest

golangci-lint: ## install linter
	brew install golangci/tap/golangci-lint
	brew upgrade golangci/tap/golangci-lint

clean-go: ## uninstall go
	brew uninstall go golangci/tap/golangci-lint

get-python: py-env-manager pyenv-global ## install python and make it available

py-env-manager: ## install python env
	brew install pyenv
	brew install pipenv

pyenv-global: ## install python utilities
	pyenv install $(PY_VERSION)
	pyenv local $(PY_VERSION)
	pip install --upgrade pip setuptools
	pip install -r requirements.txt

pyenv-install-requirements: ## upgrade python requirements
	pip install --upgrade pip setuptools
	pip freeze > $(PROJECT)/tmp/requirements_backup.txt
	pip install --upgrade -r requirements.txt

clean-pyenv: ## uninstall python
	pyenv uninstall $(PY_VERSION)
	brew uninstall pipenv
	brew uninstall pyenv
	rm -rf $(PYENV_ROOT)

get-ruby: ruby ## install ruby and make it available

rbenv-global: ## install ruby
	brew install ruby
	gem install solargraph rubocop

clean-rbenv: ## uninstall ruby
	brew uninstall ruby

get-typescript: node typescript ## install typescript and make it available

node: ## install node
	mkdir -p $(NVM_DIR)
	curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/$(NVM_VERSION)/install.sh | bash
	. $(NVM_DIR)/nvm.sh
	nvm install --lts
	npm i -g eslint csslint

typescript: ## install typescript
	npm i -g typescript typescript-formatter typescript-eslint typescript-language-server ts-node

clean-node: ## uninstall node
	rm -rf $(NVM_DIR) $(NPM_ROOT)

get-rust: rust rust-components rust-crates rust-analyzer ## install rust and make it available

rust: ## install rust
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

rust-update: ## update rust
	rustup install stable
	rustc --version
	rustup override set nightly
	rustup update nightly
	cargo -V

rust-components: ## install rust components
	rustup update
	rustup toolchain add nightly
	rustup component add rust-src
	rustup component add rustfmt
	rustup component add clippy
	rustup component add llvm-tools-preview

rust-crates: ## install rust tools
	cargo install cargo-watch
	cargo install cargo-edit
	cargo install cargo-update
	cargo install cargo-tree
	cargo install cargo-modules
	cargo install cargo-benchcmp
	cargo install cargo-expand
	cargo install cargo-outdated
	cargo install cargo-xtask
	cargo install evcxr_repl
	cargo install cargo-binutils
	cargo install cargo-make
	brew install fd

rust-analyzer:
	brew install rust-analyzer

lldb-mi:
	ghq get lldb-tools/lldb-mi
	export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
	export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"
	export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
	cmake .
	cmake --build .
	ln -s $(PWD)/src/lldb-mi /usr/local/bin/

clean-rust: ## uninstall rust
	rustup self uninstall

get-lisp: lisp ## install lisp and make it available

lisp: ## install lisp
	brew install roswell
	ros setup
	ros install $(CL_COMPILER)/$(CL_COMPILER_VERSION)

clean-lisp: ## uninstall lisp
	ros list installed 2>/dev/null | xargs -I X ros delete X
	brew uninstall roswell
	rm -rf $(ROS_ROOT)

get-haskell: haskell haskell-packages ## install haskell and make it available

haskell: ## install haskell
	curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
	curl -sSL https://get.haskellstack.org/ | sh

haskell-packages: ## install haskell utilities
	stack install hlint
	stack install implicit-hie

clean-haskell: ## uninstall haskell
	rm -rf ~/.stack
	rm -f /usr/local/bin/stack

get-clang: clang-tools ## install c

clang-tools: ## install c utils
	brew update
	brew install llvm
	brew install clang-format

clean-clang-tools: ## uninstall c utils
	brew uninstall clang-format
	brew uninstall ccls

markdown: ruby ## install markdown utilities
	gem install github-markup commonmarker

clean-markdown: ## uninstall markdown utilities
	gem uninstall github-markup commonmarker

get-flutter: ## install flutter
	mkdir -p $(FLUTTER_ROOT)
	cd $(FLUTTER_ROOT)
	wget https://storage.googleapis.com/flutter_infra_release/releases/stable/macos/flutter_macos_2.10.3-stable.zip
	unzip flutter_macos_2.10.3-stable.zip

clean-flutter: ## uninstall flutter
	rm -rf $(FLUTTER_ROOT)
