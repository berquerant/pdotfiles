git-global: ## modify global git settings
	@echo you should set user.email and user.name
	git config --global color.ui auto
	git config --global diff.renames true
	git config --global core.attributesfile ~/.gitattributes
	git config --global log.abbrevCommit true
	git config --global core.ignorecase false
	git config --global core.autocrlf false

brew-update: ## update brew packages
	brew update
	brew upgrade
	brew cleanup

go: ## install go
	bin/install-target.sh go

go-update: ## update go
	bin/install-target.sh go 1

python: ## install python
	bin/install-target.sh python

python-update: ## update python
	bin/install-target.sh python 1

ruby: ## install ruby
	bin/install-target.sh ruby

ruby-update: ## update ruby
	bin/install-target.sh ruby 1

node: ## install node
	bin/install-target.sh node

node-update: ## update node
	bin/install-target.sh node 1

rust: ## install rust
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
	bin/install-requirements.sh rust

rust-update: ## update rust
	bin/install-requirements.sh rust

lldb-mi: ## install lldb-mi
	bin/install-via-git.sh lldb-mi

zls: ## install zls
	bin/install-via-git.sh zls

zig-update: ## update zls
	bin/install-via-git.sh zls --update
