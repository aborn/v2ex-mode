EMACS ?= emacs
CASK ?= cask

ELPA_DIR = \
	.cask/$(shell $(EMACS) -Q --batch --eval '(princ emacs-version)')/elpa

.PHONY: travis-ci

travis-ci: elpa
	$(CASK) exec $(EMACS) -batch -Q -l ci/v2ex-mode-init.el

clean:
	        rm -f *.elc

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	touch $@
