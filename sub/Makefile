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
	bin/install-go.sh

go-update: go-update-install go-packages

go-update-install:
	IVG_FORCE_UPDATE=1 bin/install-go.sh

go-packages: ## install golang utilities
	go install golang.org/x/tools/gopls@latest
	go install golang.org/x/tools/cmd/goimports@latest
	go install github.com/go-delve/delve/cmd/dlv@latest
	go install github.com/google/gops@latest

golangci-lint: ## install linter
	brew install golangci/tap/golangci-lint
	brew upgrade golangci/tap/golangci-lint

get-python: py-env-manager pyenv-global ## install python and make it available

py-env-manager: ## install python env
	bin/install-python.sh
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

pyenv-force-update:
	IVG_FORCE_UPDATE=1 bin/install-python.sh

pyenv-update: pyenv-force-update pyenv-upgrade-requirements

get-ruby: ruby ruby-gems  ## install ruby and make it available

ruby: ## install ruby
	bin/install-ruby.sh

ruby-gems:
	gem install bundler solargraph rubocop

ruby-update:
	IVG_FORCE_UPDATE=1 bin/install-ruby.sh
	gem update

get-node: install-node node-dependencies ## install node

install-node:
	bin/install-node.sh

node-dependencies:
	npm update -g npm
	npm i -g eslint csslint typescript typescript-formatter typescript-eslint typescript-language-server ts-node
	npm update -g

node-update: node-dependencies
	IVG_FORCE_UPDATE=1 bin/install-node.sh

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

terraform: ## install terraform
	brew tap hashicorp/tap
	brew install tfenv
	brew install hashicorp/tap/terraform-ls

get-zig: zig ## install zig

zig: ziglang zls ## install zig

ziglang:
	bin/install-zig.sh

zls:
	bin/install-zls.sh

zig-update:
	IVG_FORCE_UPDATE=1 bin/install-zig.sh
	IVG_FORCE_UPDATE=1 bin/install-zls.sh
