SUB_TARGET_LIST := sub/target.list
TARGET_BULK := bin/install-target-bulk.sh
IVG_SH := bin/install-via-git.sh

.PHONY: sub
sub: sub-target rust zig ## install sub dependencies
sub-update: sub-target-update rust-update zig-update ## update sub dependencies
sub-retry: sub-target-retry rust-retry zig-retry ## retry to install sub dependencies

sub-target: ## install sub targets
	cat $(SUB_TARGET_LIST) | $(TARGET_BULK)

sub-target-update: ## update sub targets
	cat $(SUB_TARGET_LIST) | $(TARGET_BULK) --update

sub-target-retry: ## retry to install sub targets
	cat $(SUB_TARGET_LIST) | $(TARGET_BULK) --retry

rust: ## install rust
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
	bin/install-requirements.sh rust

rust-update: ## update rust
	bin/install-requirements.sh rust

rust-retry: rust ## retry to install rust

zig: ## install zig
	$(IVG_SH) zls

zig-update: ## update zig
	$(IVG_SH) zls --update

zig-retry: ## retry to install zig
	$(IVG_SH) zls --retry
