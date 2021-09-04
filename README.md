markdown-toc
============

[![Build Status](https://travis-ci.com/ardumont/markdown-toc.png?branch=master)](https://travis-ci.com/ardumont/markdown-toc) [![Coverage Status](https://coveralls.io/repos/ardumont/markdown-toc/badge.svg?branch=master&service=github)](https://coveralls.io/github/ardumont/markdown-toc?branch=master) [![MELPA Stable](http://stable.melpa.org/packages/markdown-toc-badge.svg)](http://stable.melpa.org/#/markdown-toc) [![MELPA](http://melpa.org/packages/markdown-toc-badge.svg)](http://melpa.org/#/markdown-toc) [![SWH](https://archive.softwareheritage.org/badge/origin/https://github.com/ardumont/markdown-toc/)](https://archive.softwareheritage.org/browse/origin/?origin_url=https://github.com/ardumont/markdown-toc)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Use](#use)
    - [Create](#create)
    - [User toc manipulation](#user-toc-manipulation)
    - [Update](#update)
    - [Create elsewhere](#create-elsewhere)
    - [Remove](#remove)
    - [Customize](#customize)
    - [Minor mode](#minor-mode)
- [Install](#install)
    - [emacs package repository](#emacs-package-repository)
        - [Setup](#setup)
            - [melpa stable](#melpa-stable)
            - [melpa](#melpa)
        - [Install](#install-1)
    - [emacs-lisp file](#emacs-lisp-file)
- [Inspiration](#inspiration)

<!-- markdown-toc end -->

A simple mode to create TOC in a well-formed markdown file.

Note that the TOC is well-formed if the markdown is (cf. #15).

# Use

## Create

Inside a markdown file, the first time, place yourself where you want to insert the TOC:

<kbd>M-x markdown-toc-generate-toc</kbd>

This will compute the TOC and insert it at current position.

You can also execute: <kbd>M-x markdown-toc-generate-or-refresh-toc</kbd> to either
gnerate a TOC when none exists or refresh the currently existing one.

Here is one possible output:

```markdown
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
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
```

## User toc manipulation

If the user would want to enhance the generated toc, (s)he could use
the following function markdown-toc-user-toc-structure-manipulation-fn:

It expects as argument the toc-structure markdown-toc uses to generate
the toc. The remaining code expects a similar structure.

Example:

``` lisp
'((0 . "some markdown page title")
  (0 . "main title")
  (1 . "Sources")
  (2 . "Marmalade (recommended)")
  (2 . "Melpa-stable")
  (2 . "Melpa (~snapshot)")
  (1 . "Install")
  (2 . "Load org-trello")
  (2 . "Alternative")
  (3 . "Git")
  (3 . "Tar")
  (0 . "another title")
  (1 . "with")
  (1 . "some")
  (1 . "heading"))
```

So for example, as asked in #16, one could drop the first element:

``` lisp
(custom-set-variables '(markdown-toc-user-toc-structure-manipulation-fn 'cdr))
```

Or drop all h1 titles... or whatever:

``` lisp
(require 'dash)
(custom-set-variables '(markdown-toc-user-toc-structure-manipulation-fn
  (lambda (toc-structure)
  (-filter (lambda (l) (let ((index (car l)))
                    (<= 1 index)))
           toc-structure)))
```

## Update

To update the existing TOC, simply execute:
<kbd>M-x markdown-toc-refresh-toc</kbd>

This will update the current TOC.

## Create elsewhere

To create another updated TOC elsewhere, execute <kbd>M-x
markdown-toc-generate-toc</kbd> again, this will remove the old TOC and
insert the updated one from where you stand.

## Remove

To remove a TOC, execute <kbd>M-x markdown-toc-delete-toc</kbd>.

## Customize

Currently, you can customize the following:

* `markdown-toc-header-toc-start`
* `markdown-toc-header-toc-title`
* `markdown-toc-header-toc-end`
*  markdown-toc-indentation-space

Customize them as following format:

``` lisp
(custom-set-variables
 '(markdown-toc-header-toc-start "<!-- customized start-->")
 '(markdown-toc-header-toc-title "**customized title**")
 '(markdown-toc-header-toc-end "<!-- customized end -->")
 '(markdown-toc-indentation-space 4))
```

## Minor mode

markdown-toc-mode provides a minor mode with the following default binding:

```
(setq markdown-toc-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c m .") 'markdown-toc-follow-link-at-point)
        (define-key map (kbd "C-c m t") 'markdown-toc-generate-or-refresh-toc)
        (define-key map (kbd "C-c m d") 'markdown-toc-delete-toc)
        (define-key map (kbd "C-c m v") 'markdown-toc-version)
        map))
```

To (de)activate this in an org file: /M-x markdown-toc-mode/

You can also use emacs to setup your own bindings.

# Install

## emacs package repository

You need to add melpa or melpa-stable package repository before installing it.

### Setup

#### melpa stable

``` lisp
(require 'package)
(add-to-list 'package-archives '("melpa-stable" .
                                 "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)
```

Then hit <kbd>M-x eval-buffer</kbd> to evaluate the buffer's contents.

#### melpa

``` lisp
(require 'package)
(add-to-list 'package-archives '("melpa" .
                                 "http://melpa.milkbox.net/packages/"))
(package-initialize)
```

Then hit <kbd>M-x eval-buffer</kbd> to evaluate the buffer's contents.

### Install

<kbd>M-x package-install RET markdown-toc RET</kbd>

## emacs-lisp file

Retrieve the markdown-toc.el https://github.com/ardumont/markdown-toc/releases.

Then hit <kbd>M-x package-install-file RET markdown-toc.el RET</kbd>

# Inspiration

https://github.com/thlorenz/doctoc

The problem I had with doctoc is the installation process.
I do not want to install the node tools just for this.
