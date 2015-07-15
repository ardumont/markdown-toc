markdown-toc [![Build Status](https://travis-ci.org/ardumont/markdown-toc.png?branch=master)](https://travis-ci.org/ardumont/markdown-toc) [![Coverage Status](https://coveralls.io/repos/ardumont/markdown-toc/badge.svg?branch=master&service=github)](https://coveralls.io/github/ardumont/markdown-toc?branch=master) [![MELPA Stable](http://stable.melpa.org/packages/markdown-toc-badge.svg)](http://stable.melpa.org/#/markdown-toc) [![MELPA](http://melpa.org/packages/markdown-toc-badge.svg)](http://melpa.org/#/markdown-toc)
============

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Use](#use)
    - [Create](#create)
    - [Update](#update)
    - [Create elsewhere](#create-elsewhere)
- [Install](#install)
    - [emacs package repository](#emacs-package-repository)
        - [Setup](#setup)
            - [melpa stable](#melpa-stable)
            - [melpa](#melpa)
            - [marmalade](#marmalade)
        - [Install](#install)
    - [emacs-lisp file](#emacs-lisp-file)
- [Inspiration](#inspiration)

<!-- markdown-toc end -->

A simple mode to create TOC in a markdown file.

# Use

## Create

Inside a markdown file, the first time, place yourself where you want to insert the TOC:

<kbd>M-x markdown-toc/generate-toc</kbd>

This will compute the TOC and insert it at current position.

Here is one possible output:

```markdown
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [some markdown page title](#some-markdown-page-title)
- [main title](#main-title)
	- [Sources](#sources)
		- [Marmalade (recommended)](#marmalade-recommended)
		- [Melpa-stable](#melpa-stable)
		- [Melpa (~snapshot)](#melpa-~snapshot)
	- [Install](#install)
		- [Load org-trello](#load-org-trello)
		- [Alternative](#alternative)
			- [Git](#git)
			- [Tar](#tar)
- [another title](#another-title)
	- [with](#with)
	- [some](#some)
	- [heading](#heading)

<!-- markdown-toc end -->
```

## Update

To update the existing TOC, simply execute again: <kbd>C-u M-x markdown-toc/generate-toc</kbd>

This will update the current TOC.

## Create elsewhere

To create another updated TOC elsewhere, execute again <kbd>M-x markdown-toc/generate-toc</kbd>, this will remove the old TOC and insert the updated one from where you stand.

# Install

## emacs package repository

You need to add melpa or melpa-stable package repository before installing it.

### Setup

#### melpa stable

``` lisp
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)
```

Then hit <kbd>M-x eval-buffer</kbd> to evaluate the buffer's contents.

#### melpa

``` lisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
```

Then hit <kbd>M-x eval-buffer</kbd> to evaluate the buffer's contents.

#### marmalade

``` lisp
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
```

### Install

<kbd>M-x package-install RET markdown-toc RET</kbd>

## emacs-lisp file

Retrieve the markdown-toc.el https://github.com/ardumont/markdown-toc/releases.

Then hit <kbd>M-x package-install-file RET markdown-toc.el RET</kbd>

# Inspiration

https://github.com/thlorenz/doctoc

The problem I had with doctoc is the installation process.
I do not want to install the node tools just for this.
