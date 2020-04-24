MODE_NAME=markdown-toc
VERSION=$$(grep "^;; Version: " $(MODE_NAME).el | cut -f3 -d' ')
PACKAGE_FOLDER=$(MODE_NAME)-$(VERSION)
ARCHIVE=$(PACKAGE_FOLDER).tar
EMACS=emacs
CASK ?= cask
LANG=en_US.UTF-8

.PHONY: clean

pr:
	hub pull-request -b ardumont:master

build:
	${CASK} build

clean-dist:
	rm -rf dist/

clean: clean-dist
	rm -rf *.tar
	${CASK} clean-elc

install:
	${CASK} install

test-init:
	${CASK} exec ert-runner init

test: clean
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
