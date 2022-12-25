git-global: ## modify global git settings
	@echo you should set user.email and user.name
	git config --global color.ui auto
	git config --global diff.renames true
	git config --global core.attributesfile ~/.gitattributes
	git config --global log.abbrevCommit true
	git config --global core.ignorecase false
	git config --global core.autocrlf false

brew-update:
	brew update
	brew upgrade
	brew cleanup

get-go: go go-packages golangci-lint ## install go and make it available

go: ## install go
	git clone https://github.com/syndbg/goenv.git $(GOENV_ROOT)
	goenv install $(GO_VERSION)

go-update: go-packages

go-packages: ## install golang utilities
	go install golang.org/x/tools/gopls@latest
	go install golang.org/x/tools/cmd/goimports@latest
	go install github.com/go-delve/delve/cmd/dlv@latest
	go install github.com/google/gops@latest

golangci-lint: ## install linter
	brew install golangci/tap/golangci-lint
	brew upgrade golangci/tap/golangci-lint

clean-go: ## uninstall go
	brew uninstall goenv golangci/tap/golangci-lint
	rm -rf $(GOENV_ROOT)

get-python: py-env-manager pyenv-global ## install python and make it available

py-env-manager: ## install python env
	brew install pyenv
	brew install pipenv

pyenv-global: ## install python utilities
	pyenv install $(PY_VERSION)
	pyenv local $(PY_VERSION)
	pip install --upgrade pip setuptools
	pip install -r requirements.txt

pyenv-upgrade-requirements: ## upgrade python requirements
	pip install --upgrade pip setuptools
	pip freeze > $(PJTMP)/requirements_backup.txt
	pip install --upgrade -r requirements.txt
	pip freeze > $(PJTMP)/requirements.txt

pyenv-update: pyenv-upgrade-requirements

clean-pyenv: ## uninstall python
	brew uninstall pipenv
	brew uninstall pyenv
	rm -rf $(PYENV_ROOT)

get-ruby: ruby ## install ruby and make it available

ruby: ## install ruby
	brew install ruby
	gem install solargraph rubocop

clean-ruby: ## uninstall ruby
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

node-update:
	npm update -g npm
	npm update --location=global

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

rust-analyzer:
	brew install rust-analyzer

lldb-mi:
	bin/install-lldb-mi.sh

clean-rust: ## uninstall rust
	rustup self uninstall

get-clang: clang-tools ## install c

clang-tools: ## install c utils
	brew update
	brew install llvm
	brew install clang-format

clean-clang-tools: ## uninstall c utils
	brew uninstall clang-format
	brew uninstall ccls

markdown: get-ruby ## install markdown utilities
	gem install github-markup commonmarker

clean-markdown: ## uninstall markdown utilities
	gem uninstall github-markup commonmarker

clojure: ## install clojure
	brew install clojure/tools/clojure leiningen clojure-lsp/brew/clojure-lsp-native

clean-clojure: ## uninstall clojure
	brew uninstall clojure/tools/clojure leiningen clojure-lsp/brew/clojure-lsp-native

terraform: ## install terraform
	brew tap hashicorp/tap
	brew install tfenv
	brew install hashicorp/tap/terraform-ls

clean-terraform: ## uninstall terraform
	brew uninstall tfenv
	brew uninstall hashicorp/tap/terraform-ls

get-zig: zig zig-ls  ## install zig

zig: ## install zig
	bin/install-zig.sh

zig-update: zig
