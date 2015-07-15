MODE_NAME=markdown-toc
VERSION=$$(grep "^;; Version: " $(MODE_NAME).el | cut -f3 -d' ')
PACKAGE_FOLDER=$(MODE_NAME)-$(VERSION)
ARCHIVE=$(PACKAGE_FOLDER).tar
EMACS=emacs

.PHONY: clean

pr:
	hub pull-request -b ardumont:master

build:
	cask build

clean-dist:
	rm -rf dist/

clean: clean-dist
	rm -rf *.tar
	cask clean-elc

install:
	cask install

test-init:
	cask exec ert-runner init

test: clean
	cask exec ert-runner

pkg-file:
	cask pkg-file

pkg-el: pkg-file
	cask package

package: clean pkg-el
	cp dist/$(ARCHIVE) .
	make clean-dist

info:
	cask info

release:
	./release.sh $(VERSION)

install-cask:
	curl -fsSkL https://raw.github.com/cask/cask/master/go | python
