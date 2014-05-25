mardown-toc [![Build Status](https://travis-ci.org/ardumont/markdown-toc.png?branch=master)](https://travis-ci.org/ardumont/markdown-toc)
===========

A simple mode to create TOC in a markdown file.

<!-- markdown-toc start - Don't edit this section. Run M-x mardown-toc/generate-toc again -->
**table of Contents**

- [Use](#use)
- [Install](#install)
- [TODO](#todo)
<!-- markdown-toc end -->

# Use

Inside a markdown.
<kbd>M-x markdown-toc/generate-toc</kbd>

This will compute the TOC at insert it at current position.

Here is a possible output:

```markdown
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
```

# Install

<kbd>M-x package-install RET markdown-toc RET</kbd>

# Inspiration

https://github.com/thlorenz/doctoc

The problem I had with doctoc is the installation process.
I do not want to install the node tools just for this.
