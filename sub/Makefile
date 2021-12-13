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
	git clone https://github.com/syndbg/goenv.git $(GOENV_ROOT)

go-packages: ## install golang utilities
	GO111MODULE=on go install golang.org/x/tools/gopls@latest
	go install -u golang.org/x/tools/cmd/goimports
	go install github.com/x-motemen/gore/cmd/gore@latest
	go install github.com/go-delve/delve/cmd/dlv@latest
	go install github.com/google/gops@latest

golangci-lint: ## install linter
	brew install golangci/tap/golangci-lint
	brew upgrade golangci/tap/golangci-lint

clean-go: ## uninstall go
	rm -rf `goenv root`

get-python: py-env-manager pyenv-global ## install python and make it available

py-env-manager: ## install
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

get-typescript: node typescript ## install typescript and make it available

node: $(NVM_DIR)/nvm.sh ## install node
	curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/$(NVM_VERSION)/install.sh | bash
	nvm install --lts
	npm install -g eslint csslint

typescript: ## install typescript
	npm i -g typescript typescript-formatter tslint typescript-language-server ts-node typescript

clean-node: ## uninstall node
	rm -rf $(NVM_ROOT) $(NPM_ROOT)

get-rust: rust rust-components rust-crates ## install rust and make it available

rust: ## install rust
	curl https://sh.rustup.rs -sSf | sh

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
	cargo +nightly install racer
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
	cd $(GHQ_ROOT) && \
	git clone https://github.com/rust-analyzer/rust-analyzer.git && \
	cd rust-analyzer && \
	cargo xtask install

clean-rust: ## uninstall rust
	rustup self uninstall

get-scala: scala java scala-tools ## install scala and make it available

scala: ## install scala
	brew install scala
	brew install sbt

java: ## install java
	brew tap AdoptOpenJDK/openjdk
	brew cask install adoptopenjdk/openjdk/adoptopenjdk8
	java -version # remove some jdk from /Library/Java/JavaVirtualMachines/ if you see other java version
	brew install jadx

clean-scala: ## uninstall scala
	brew uninstall sbt
	brew uninstall scala
	brew cask uninstall adoptopenjdk/openjdk/adoptopenjdk8

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

markdown: ## install markdown utilities
	gem install github-markup commonmarker

clean-markdown: ## uninstall markdown utilities
	gem uninstall github-markup commonmarker
