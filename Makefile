MODE_NAME=markdown-toc
VERSION=$$(grep "^;; Version: " $(MODE_NAME).el | cut -f3 -d' ')
PACKAGE_FOLDER=$(MODE_NAME)-$(VERSION)
ARCHIVE=$(PACKAGE_FOLDER).tar
EMACS=emacs
CASK ?= cask
LANG=en_US.UTF-8

.PHONY: clean

activate:
	nix develop

pr:
	hub pull-request -b ardumont:master

build:
	${CASK} build

clean-cask:
	[ -d .cask ] && rm -rf .cask/ || echo

clean-dist:
	[ -d dist ] && rm -rf dist/ || echo

clean: clean-dist clean-cask
	rm -rf ${ARCHIVE}
	${CASK} clean-elc

install:
	[ ! -d .cask ] && ${CASK} install || echo

test: install
	${CASK} exec ert-runner

pkg-file:
	${CASK} pkg-file

pkg-el: pkg-file
	${CASK} package

package: clean pkg-el
	cp dist/$(ARCHIVE) .
	make clean-dist

info:
	${CASK} info

release:
	./release.sh $(VERSION)

version:
	@echo "application $(PACKAGE): $(VERSION)\npackage: $(ARCHIVE)"
