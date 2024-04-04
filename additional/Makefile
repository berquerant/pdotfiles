ADDITIONAL_IVG_LIST := additional/ivg.list
IVG_BULK := bin/install-via-git-bulk.sh

.PHONY: additional
additional: ## install additional dependencies
	cat $(ADDITIONAL_IVG_LIST) | $(IVG_BULK)

additional-update: ## update additional dependencies
	cat $(ADDITIONAL_IVG_LIST) | $(IVG_BULK) --update

additional-retry: ## retry to install additional dependencies
	cat $(ADDITIONAL_IVG_LIST) | $(IVG_BULK) --retry
