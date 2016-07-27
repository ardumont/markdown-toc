;;; markdown-toc.el --- A simple TOC generator for markdown file
;; Copyright (C) 2014-2016 Antoine R. Dumont

;; Author: Antoine R. Dumont
;; Maintainer: Antoine R. Dumont
;; URL: http://github.com/ardumont/markdown-toc
;; Created: 24th May 2014
;; Version: 0.1.2
;; Keywords: markdown, toc, tools,
;; Package-Requires: ((markdown-mode "2.1") (dash "2.11.0") (s "1.9.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Generate a TOC from a markdown file: M-x markdown-toc-generate-toc
;; This will compute the TOC at insert it at current position.
;; Update existing TOC: C-u M-x markdown-toc-generate-toc

;; Here is a possible output:
;; <!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
;; **Table of Contents**

;; - [some markdown page title](#some-markdown-page-title)
;; - [main title](#main-title)
;;     - [Sources](#sources)
;;         - [Marmalade (recommended)](#marmalade-recommended)
;;         - [Melpa-stable](#melpa-stable)
;;         - [Melpa (~snapshot)](#melpa-~snapshot)
;;     - [Install](#install)
;;         - [Load org-trello](#load-org-trello)
;;     - [Alternative](#alternative)
;;         - [Git](#git)
;;         - [Tar](#tar)
;; - [another title](#another-title)
;;     - [with](#with)
;;     - [some](#some)
;; - [heading](#heading)
;;
;; <!-- markdown-toc end -->

;; Install - M-x package-install RET markdown-toc RET

;;; Code:

(require 's)
(require 'dash)
(require 'markdown-mode)

(defconst markdown-toc--toc-version "0.1.2" "Current version installed.")

(defgroup markdown-toc nil
  "A simple TOC generator for markdown file."
  :group 'markdown)

(defun markdown-toc-log-msg (args)
  "Log message ARGS."
  (apply #'message (format "markdown-toc - %s" (car args)) (cdr args)))

;;;###autoload
(defun markdown-toc-version ()
  "Markdown-toc version."
  (interactive)
  (message "markdown-toc version: %s" markdown-toc--toc-version))

(defalias 'markdown-toc/version 'markdown-toc-version)

(defun markdown-toc--compute-toc-structure-from-level (level menu-index)
  "Given a LEVEL and a MENU-INDEX, compute the toc structure."
  (when menu-index
    (let* ((fst   (car menu-index))
           (tail  (cdr menu-index))
           (ttail (if (integerp tail) nil (cdr tail))))
      (cons `(,level . ,fst)
            (--mapcat
             (markdown-toc--compute-toc-structure-from-level (+ 1 level) it)
             ttail)))))

(defun markdown-toc--compute-toc-structure (imenu-index)
  "Given a IMENU-INDEX, compute the TOC structure."
  (--mapcat (markdown-toc--compute-toc-structure-from-level 0 it) imenu-index))

(defun markdown-toc--symbol (sym n)
  "Compute the repetition of a symbol SYM N times as a string."
  (--> n
       (-repeat it sym)
       (s-join "" it)))

(defconst markdown-toc--protection-symbol "09876543214b825dc642cb6eb9a060e54bf8d69288fbee49041234567890"
  "Implementation detail to protect some punctuation characters
  when converting to link.")

(defun markdown-toc--to-link (title)
  "Given a TITLE, return the markdown link associated."
  (format "[%s](#%s)" title
          (->> title
               downcase
               (replace-regexp-in-string "-" markdown-toc--protection-symbol)
               (replace-regexp-in-string "[[:punct:]]" "")
               (replace-regexp-in-string markdown-toc--protection-symbol "-")
               (s-replace " " "-"))))

(defun markdown-toc--to-markdown-toc (level-title-toc-list)
  "Given LEVEL-TITLE-TOC-LIST, a list of pair level, title, return a TOC string."
  (->> level-title-toc-list
       (--map (let ((nb-spaces (* 4 (car it)))
                    (title     (cdr it)))
                (format "%s- %s" (markdown-toc--symbol " " nb-spaces)
                        (markdown-toc--to-link title))))
       (s-join "\n")))

(defcustom markdown-toc-header-toc-start
  "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->"
  "Beginning delimiter comment."
  :group 'markdown-toc)

(defcustom markdown-toc-header-toc-title
  "**Table of Contents**"
  "Title comment on TOC header."
  :group 'markdown-toc)

(defcustom markdown-toc-header-toc-end
  "<!-- markdown-toc end -->"
  "Ending delimiter comment."
  :group 'markdown-toc)

(defun markdown-toc--toc-already-present-p ()
  "Determine if a TOC has already been generated.
Return the end position if it exists, nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward markdown-toc-header-toc-start nil t)))

(defun markdown-toc--toc-start ()
  "Compute the toc's starting point."
  (save-excursion
    (goto-char (markdown-toc--toc-already-present-p))
    (point-at-bol)))

(defun markdown-toc--toc-end ()
  "Compute the toc's end point."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward markdown-toc-header-toc-end nil t)))

(defun markdown-toc--generate-toc (toc-structure)
  "Given a TOC-STRUCTURE, compute a new toc."
  (-> toc-structure
      markdown-toc--to-markdown-toc
      markdown-toc--compute-full-toc))

(defun markdown-toc--compute-full-toc (toc)
  "Given the TOC's content, compute the full toc with comments and title."
  (format "%s\n%s\n\n%s\n\n%s\n"
          markdown-toc-header-toc-start
          markdown-toc-header-toc-title
          toc
          markdown-toc-header-toc-end))

(defcustom markdown-toc-user-toc-structure-manipulation-fn
  (lambda (toc-structure) toc-structure)
  "User crafted function to manipulate toc-structure as user sees fit.

The toc-structure has the following form:
'((0 . \"some markdown page title\")
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
    '(markdown-toc-user-toc-structure-manipulation-fn 'cdr))

Default to identity function (do nothing)."
  :group 'markdown-toc)

;;;###autoload

(defun markdown-toc-generate-toc (&optional replace-toc-p)
  "Generate a TOC for markdown file at current point.
Deletes any previous TOC.
If called interactively with prefix arg REPLACE-TOC-P, replaces previous TOC."
  (interactive "P")
  (save-excursion
    (when (markdown-toc--toc-already-present-p)
      ;; when toc already present, remove it
      (let ((region-start (markdown-toc--toc-start))
            (region-end   (markdown-toc--toc-end)))
        (delete-region region-start (1+ region-end))
        (when replace-toc-p
          (goto-char region-start))))
    (->> (markdown-imenu-create-nested-index)
         markdown-toc--compute-toc-structure
         (funcall markdown-toc-user-toc-structure-manipulation-fn)
         markdown-toc--generate-toc
         insert)))

(defalias 'markdown-toc/generate-toc 'markdown-toc-generate-toc)

(defun markdown-toc--bug-report ()
  "Compute the bug report for the user to include."
  (->> `("Please:"
         "- Describe your problem with clarity and conciceness (cf. https://www.gnu.org/software/emacs/manual/html_node/emacs/Understanding-Bug-Reporting.html)"
         "- Explicit your installation choice (melpa, marmalade, el-get, tarball, git clone...)."
         "- Report the following message trace inside your issue."
         ""
         "System information:"
         ,(format "- system-type: %s" system-type)
         ,(format "- locale-coding-system: %s" locale-coding-system)
         ,(format "- emacs-version: %s" (emacs-version))
         ,(format "- markdown-toc version: %s" markdown-toc--toc-version)
         ,(format "- markdown-toc path: %s" (find-library-name "markdown-toc")))
       (s-join "\n")))

(defun markdown-toc-bug-report (&optional open-url)
  "Display a bug report message.
When OPEN-URL is filled, with universal argument (`C-u') is used,
opens new issue in markdown-toc's github tracker."
  (interactive "P")
  (when open-url
    (browse-url "https://github.com/ardumont/markdown-toc/issues/new"))
  (markdown-toc-log-msg (list (markdown-toc--bug-report))))

(provide 'markdown-toc)
;;; markdown-toc.el ends here
