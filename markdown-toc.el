;;; markdown-toc.el --- A simple TOC generator for markdown file
;; Copyright (C) 2014-2020 Antoine R. Dumont (@ardumont)

;; Author: Antoine R. Dumont (@ardumont)
;; Maintainer: Antoine R. Dumont (@ardumont)
;; URL: http://github.com/ardumont/markdown-toc
;; Created: 24th May 2014
;; Version: 0.1.5
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
;; <!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
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

(defconst markdown-toc--toc-version "0.1.5" "Current version installed.")

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

(defcustom markdown-toc-indentation-space 4
  "Let the user decide the indentation level."
  :group 'markdown-toc
  :type 'integer)

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
  :group 'markdown-toc
  :type 'function)

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

(defconst markdown-toc--dash-protection-symbol "09876543214b825dc642cb6eb9a060e54bf8d69288fbee49041234567890"
  "Implementation detail to protect the - characters
  when converting to link.")

(defconst markdown-toc--underscore-protection-symbol "afec96cafb7bc4b0e216bfe86db4bd6c4aab44bca19dd9999b11e162f595d711"
  "Implementation detail to protect the `_` characters
  when converting to link.")

(defun markdown-toc--to-link (title &optional count)
  "Given a TITLE, return the markdown link associated."

  (let ((count (if count count 0)))
    (format "[%s](#%s%s)" title
            (->> title
                 s-trim
                 downcase
                 (s-replace "-" markdown-toc--dash-protection-symbol)
                 (s-replace "_" markdown-toc--underscore-protection-symbol)
                 (replace-regexp-in-string "[[:punct:]]" "")
                 (s-replace markdown-toc--dash-protection-symbol "-")
                 (s-replace markdown-toc--underscore-protection-symbol "_")
                 (s-replace " " "-"))
            (if (> count 0)
                (concat "-" (number-to-string count))
              ""))))

(defun markdown--count-duplicate-titles (toc-structure)
  "Counts the number of times each title appeared in the toc structure and adds
it to the TOC structure."
  (-map-indexed (lambda (index n)
          (let* ((indent (car n))
                (title (cdr n))
                (count (--count (string= title (cdr it))
                         (-take (+ index 1) toc-structure))))
            (list indent title (- count 1))))
    toc-structure))

(defun markdown-toc--to-markdown-toc (level-title-toc-list)
  "Given LEVEL-TITLE-TOC-LIST, a list of pair level, title, return a TOC string."
  (->> level-title-toc-list
       markdown--count-duplicate-titles
       (--map (let ((nb-spaces (* markdown-toc-indentation-space (car it)))
                    (title     (car (cdr it)))
                    (count     (car (cdr (cdr it)))))
                (format "%s%s %s"
                        (markdown-toc--symbol " " nb-spaces)
                        markdown-toc-list-item-marker
                        (markdown-toc--to-link title count))))
       (s-join "\n")))

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

(defun markdown-toc--delete-toc (&optional replace-toc-p)
  "Delets a TOC."
  (let ((region-start (markdown-toc--toc-start))
        (region-end   (markdown-toc--toc-end)))
    (delete-region region-start (1+ region-end))
    (when replace-toc-p
          (goto-char region-start))))

(defun markdown-toc--compute-full-toc (toc)
  "Given the TOC's content, compute the full toc with comments and title."
  (format "%s\n%s\n\n%s\n\n%s\n"
          markdown-toc-header-toc-start
          markdown-toc-header-toc-title
          toc
          markdown-toc-header-toc-end))

;;;###autoload
(defun markdown-toc-generate-toc (&optional replace-toc-p)
  "Generate a TOC for markdown file at current point.
Deletes any previous TOC.
If called interactively with prefix arg REPLACE-TOC-P, replaces previous TOC."
  (interactive "P")
  (save-excursion
    (when (markdown-toc--toc-already-present-p)
      ;; when toc already present, remove it
      (markdown-toc--delete-toc t))
    (->> (funcall imenu-create-index-function)
         markdown-toc--compute-toc-structure
         (funcall markdown-toc-user-toc-structure-manipulation-fn)
         markdown-toc--generate-toc
         insert)))

(defalias 'markdown-toc/generate-toc 'markdown-toc-generate-toc)

;;;###autoload
(defun markdown-toc-generate-or-refresh-toc ()
  "Generate a TOC for markdown file at current point or refreshes an already generated TOC."
  (interactive)
  (markdown-toc-generate-toc t))

;;;###autoload
(defun markdown-toc-refresh-toc ()
  "Refreshes an already generated TOC."
  (interactive)
  (when (markdown-toc--toc-already-present-p)
    (markdown-toc-generate-toc t)))

;;;###autoload
(defun markdown-toc-delete-toc ()
  "Deletes a previously generated TOC."
  (interactive)
  (save-excursion
    (markdown-toc--delete-toc t)))

(defun markdown-toc--read-title-out-of-link (link)
  "Extract the link title out of a markdown LINK title.
This assumes no funky stuff in the markdown link format ` - [<title>](...) `  "
  (->> link
       s-trim
       (s-chop-prefix "- [")
       (s-split "]")
       car))

(defun markdown-toc--title-level (link)
  "Determine the markdown title LINK out of its indentation.
If misindented or not prefixed by `-`, it's considered not a link
and returns nil. Otherwise, returns the level number."
  (when (s-prefix? "-" (-> link s-trim)) ;; if not, it's not a link title
    (let ((indent (->> link
                       (s-split "-")
                       car  ;; first string contains a string with empty spaces
                       ;; which should be a multiple of
                       ;; `markdown-toc-indentation-space`
                       length)))
      (when (zerop (% indent markdown-toc-indentation-space))
        (+ 1 (/ indent markdown-toc-indentation-space))))))

;;;###autoload
(defun markdown-toc-follow-link-at-point ()
  "On a given toc link, navigate to the current markdown header.
If the toc is misindented (according to markdown-toc-indentation-space`)
or if not on a toc link, this does nothing.
"
  (interactive)
  (let* ((full-title (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
         (level (markdown-toc--title-level full-title)))
    (if level ;; nil if misindented or not on a title
        (let ((title (markdown-toc--read-title-out-of-link full-title)))
          (goto-char (point-min))
          (search-forward-regexp (format "%s %s" (s-repeat level "#") title)))
      (message "markdown-toc: Not on a link (or misindented), nothing to do"))))

(defun markdown-toc--bug-report ()
  "Compute the bug report for the user to include."
  (require 'find-func)
  (->> `("Please:"
         "- Describe your problem with clarity and conciceness (cf. https://www.gnu.org/software/emacs/manual/html_node/emacs/Understanding-Bug-Reporting.html)"
         "- Explicit your installation choice (melpa, marmalade, el-get, tarball, git clone...)."
         "- Report the following message trace inside your issue."
         ""
         "System information:"
         ,(format "- system-type: %s" system-type)
         ,(format "- locale-coding-system: %s" locale-coding-system)
         ,(format "- emacs-version: %s" (emacs-version))
         ,(format "- markdown-mode path: %s" (find-library-name "markdown-mode"))
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

(defvar markdown-toc-mode-map nil "Default Bindings map for markdown-toc mode.")

(setq markdown-toc-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c m .") 'markdown-toc-follow-link-at-point)
        (define-key map (kbd "C-c m t") 'markdown-toc-generate-or-refresh-toc)
        (define-key map (kbd "C-c m d") 'markdown-toc-delete-toc)
        (define-key map (kbd "C-c m v") 'markdown-toc-version)
        map))

;;;###autoload
(define-minor-mode markdown-toc-mode
  "Functionality for generating toc in markdown file.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{markdown-toc-mode-map}"

  :init-value nil
  :lighter " mt"
  :group 'markdown-toc
  :keymap markdown-toc-mode-map)

(provide 'markdown-toc)
;;; markdown-toc.el ends here
