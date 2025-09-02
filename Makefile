EMACS ?= emacs
CASK ?= cask

LOADPATH = -L .
LOAD_PACKAGES = $(LOADPATH) -L ./test

ELFILES = probe-search.el probe-search-enhanced.el
TESTFILES = test/probe-search-test.el

.PHONY: all deps test lint clean compile install

all: deps compile test

deps:
	$(CASK) install

compile:
	$(CASK) exec $(EMACS) -Q --batch $(LOADPATH) \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(ELFILES)

test: compile
	$(CASK) exec ert-runner

lint:
	$(CASK) exec $(EMACS) -Q --batch $(LOADPATH) \
		--eval "(require 'checkdoc)" \
		--eval "(setq checkdoc-minor-mode t)" \
		--eval "(checkdoc-file \"probe-search.el\")" \
		--eval "(checkdoc-file \"probe-search-enhanced.el\")"

clean:
	rm -f *.elc
	rm -rf .cask/
	rm -f probe-search-*.tar

package: clean
	$(CASK) package

install: package
	$(EMACS) --batch -f package-initialize \
		   --eval "(package-install-file \"dist/probe-search-$(VERSION).tar\")"

autoloads:
	$(EMACS) -Q --batch $(LOADPATH) \
		--eval "(require 'autoload)" \
		--eval "(setq generated-autoload-file \"$(PWD)/probe-search-autoloads.el\")" \
		-f update-directory-autoloads .

run-probe-test:
	@echo "Testing probe command availability..."
	@which probe || (echo "ERROR: probe command not found in PATH" && exit 1)
	@probe --version

help:
	@echo "Makefile for probe.el"
	@echo ""
	@echo "Usage:"
	@echo "  make         - Install deps, compile, and test"
	@echo "  make deps    - Install dependencies"
	@echo "  make compile - Byte-compile Elisp files"
	@echo "  make test    - Run tests"
	@echo "  make lint    - Run checkdoc linting"
	@echo "  make clean   - Remove generated files"
	@echo "  make package - Create package tarball"
	@echo "  make install - Install package locally"