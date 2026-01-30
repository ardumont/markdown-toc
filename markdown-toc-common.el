;;; markdown-toc-common.el --- Common assets -*- lexical-binding: t; -*-

;;; Code:

(require 's)
(require 'dash)

(defconst markdown-toc--toc-version "0.1.6" "Current version installed.")

(defgroup markdown-toc nil
  "A simple TOC generator for markdown file."
  :group 'markdown)

(defcustom markdown-toc-list-item-marker
  "-"
  "List item marker that should be used.
Example: '-' for unordered lists or '1.' for ordered lists."
  :type '(choice
          (string :tag "Unordered list header" "-")
          (string :tag "Ordered list header" "1."))
  :group 'markdown-toc)

(defcustom markdown-toc-header-toc-start
  "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->"
  "Beginning delimiter comment."
  :group 'markdown-toc
  :type 'string)

(defcustom markdown-toc-header-toc-title
  "**Table of Contents**"
  "Title comment on TOC header."
  :group 'markdown-toc
  :type 'string)

(defcustom markdown-toc-header-toc-end
  "<!-- markdown-toc end -->"
  "Ending delimiter comment."
  :group 'markdown-toc
  :type 'string)

(defcustom markdown-toc-indentation-space 2
  "Let the user decide the indentation level."
  :group 'markdown-toc
  :type 'integer)

(defcustom markdown-toc-user-toc-structure-manipulation-fn
  (lambda (toc-structure) toc-structure)
  "User crafted function to manipulate toc-structure as user sees fit.

The toc-structure has the following form:
\\='((0 . \"some markdown page title\")
  (0 . \"main title\")
  (1 . \"Sources\")
  (2 . \"Marmalade (recommended)\")
  (2 . \"Melpa-stable\")
  (2 . \"Melpa (~snapshot)\")
  (1 . \"Install\")
  (2 . \"Load org-trello\")
  (2 . \"Alternative\")
  (3 . \"Git\")
  (3 . \"Tar\")
  (0 . \"another title\")
  (1 . \"with\")
  (1 . \"some\")
  (1 . \"heading\"))

If the user wanted to remove the first element, it could for
example define the following function:
  (custom-set-variables
    \\='(markdown-toc-user-toc-structure-manipulation-fn \\='cdr))

Default to identity function (do nothing)."
  :group 'markdown-toc
  :type 'function)

(defcustom markdown-toc-preset 'legacy
  "The algorithm preset for the markdown toc generator.

The default uses the `legacy' algorithm originated from this package.

Set to the symbol `pandoc' to compatible with pandoc html export and
with Unicode support."
  :group 'markdown-toc
  :type '(choice (const :tag "Legacy" legacy)
                 (const :tag "Pandoc" pandoc)))

(defun markdown-toc--symbol (sym n)
  "Compute the repetition of a symbol SYM N times as a string."
  (--> n
       (-repeat it sym)
       (s-join "" it)))

(provide 'markdown-toc-common)
;;; markdown-toc-common.el ends here
