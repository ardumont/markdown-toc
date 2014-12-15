# 0.0.7

- [X] Add prefix argument to `markdown-toc/generate-toc' to replace existing TOC at the same location #10
- [X] Simplify tests launcher
- [X] Add itest on toc generation when using modifier  [2/2]
  - [X] Use ert to simplify tests output reading in case of error
  - [X] Add test on the new use case from #10
- [X] Update README.md
- [X] Update version
- [X] Release notes

# 0.0.6

- [X] Fix typo on the markdown header information
- [X] Strip non-ASCII characters from heading links, to be consistent with GitHub.
- [X] Update version
- [X] Release notes

# 0.0.5

- [X] Deploy to Marmalade
- [X] Use directly markdown-toc, no need to require anything
- [X] Update documentation about installation procedure (marmalade)
- [X] Update version
- [X] Update release notes

# 0.0.4

- [X] If melpa recipe is accepted, update the README.md about how to install
- [X] Add COPYING file + licence inside markdown-toc.el
- [X] Simplify packaging (no need to use tarball nor *-pkg.el file)
- [X] Upgrade version
- [X] Release notes

# 0.0.3

- [X] Update an existing TOC
  - [X] Create a section around TOC
  - [X] Find if a TOC is already present in buffer
  - [X] Remove old TOC
  - [X] Create new one
- [X] Reference markdown-toc version
- [X] Reference the inspiration from doctoc
- [X] Upgrade version
- [X] Release notes

# 0.0.2

- [X] Prepare filesystem to deploy to melpa (avoid having tests files with the file to package)
- [X] Upgrade version + package
- [X] Release notes

# 0.0.1

- [X] Initialize Project
- [X] Initialize github repository
- [X] Cask
- [X] Generate a TOC at point
- [X] Update header documentation
- [X] Update README.md about use case
- [X] Makefile
- [X] Tests
- [X] Travis-ci
- [X] Upgrade version + package
- [X] Release notes
