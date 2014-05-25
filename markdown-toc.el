;;; markdown-toc.el --- A simple TOC generator for markdown file
;; Copyright (C) 2014 Antoine R. Dumont

;; Author: Antoine R. Dumont
;; Maintainer: Antoine R. Dumont
;; URL: http://github.com/ardumont/markdown-toc
;; Created: 24th May 2014
;; Version: 0.0.3
;; Keywords: markdown, toc, tools,
;; Package-Requires: ((markdown-mode "2.0") (dash "2.5.0") (s "1.7.0"))

;;; Commentary:

;; Inside a markdown - M-x markdown-toc/generate-toc
;; This will compute the TOC at insert it at current position.
;; Afterwards, if a TOC is already present, it will update the one present in buffer.

;; Here is a possible output:
;; <!-- markdown-toc start - Don't edit this section. Run M-x mardown-toc/generate-toc again -->
;; **table of Contents**

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
;; <!-- markdown-toc end -->

;; Install - M-x package-install RET markdown-toc RET

;;; Code:

(require 's)
(require 'dash)
(require 'markdown-mode)

(defconst *MARKDOWN-TOC/VERSION* "0.0.3" "Current version installed.")

(defun markdown-toc/version ()
  "Markdown-toc version."
  (interactive)
  (message "markdown-toc version: %s" *MARKDOWN-TOC/VERSION*))

(defun markdown-toc/--compute-toc-structure-from-level (level menu-index)
  "Given a LEVEL and a MENU-INDEX, compute the toc structure."
  (when menu-index
    (let* ((fst   (car menu-index))
           (tail  (cdr menu-index))
           (ttail (if (integerp tail) nil (cdr tail))))
      (cons `(,level . ,fst)
            (--mapcat (markdown-toc/--compute-toc-structure-from-level (+ 1 level) it) ttail)))))

(defun markdown-toc/--compute-toc-structure (imenu-index)
  "Given a IMENU-INDEX, compute the TOC structure."
  (--mapcat (markdown-toc/--compute-toc-structure-from-level 0 it) imenu-index))

(defun markdown-toc/--symbol (sym n)
  "Compute the repetition of a symbol SYM N times as a string."
  (--> n
    (-repeat it sym)
    (s-join "" it)))

(defun markdown-toc/--to-link (title)
  "Given a TITLE, return the markdown link associated."
  (format "[%s](#%s)" title (replace-regexp-in-string " " "-" (downcase title))))

(defun markdown-toc/--to-markdown-toc (level-title-toc-list)
  "Given LEVEL-TITLE-TOC-LIST, a list of pair level, title, return a TOC string."
  (->> level-title-toc-list
    (--map (let ((nb-spaces (* 4 (car it)))
                 (title     (cdr it)))
             (format "%s- %s" (markdown-toc/--symbol " " nb-spaces) (markdown-toc/--to-link title))))
    (s-join "\n")))

(defconst *markdown-toc/header-toc-start* "<!-- markdown-toc start - Don't edit this section. Run M-x mardown-toc/generate-toc again -->")
(defconst *markdown-toc/header-toc-title* "**table of Contents**")
(defconst *markdown-toc/header-toc-end*   "<!-- markdown-toc end -->")

(defun markdown-toc/--toc-already-present-p! ()
  "Determine if a TOC has already been generated.
Return the end position if it exists, nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward *markdown-toc/header-toc-start* nil t)))

(defun markdown-toc/--toc-start! ()
  "Compute the toc's starting point."
  (save-excursion
    (goto-char (markdown-toc/--toc-already-present-p!))
    (point-at-bol)))

(defun markdown-toc/--toc-end! ()
  "Compute the toc's end point."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward *markdown-toc/header-toc-end* nil t)))

(defun markdown-toc/--generate-toc (toc-index)
  "Given a TOC-INDEX, compute a new toc."
  (-> toc-index
    markdown-toc/--compute-toc-structure
    markdown-toc/--to-markdown-toc
    markdown-toc/--compute-full-toc))

(defun markdown-toc/--compute-full-toc (toc)
  "Given the TOC's content, compute the full toc with comments and title."
  (format "%s\n%s\n\n%s\n%s\n"
          *markdown-toc/header-toc-start*
          *markdown-toc/header-toc-title*
          toc
          *markdown-toc/header-toc-end*))

(defun markdown-toc/generate-toc ()
  "Generate a TOC for markdown file at current position.
Afterward, if a TOC is already present in the buffer, it will update it."
  (interactive)
  (when (markdown-toc/--toc-already-present-p!)
    ;; when toc already present, remove it
    (let ((region-start (markdown-toc/--toc-start!))
          (region-end   (markdown-toc/--toc-end!)))
      (delete-region region-start (1+ region-end))))
  ;; generate the toc
  (-> (markdown-imenu-create-index)
    markdown-toc/--generate-toc
    insert))

(provide 'markdown-toc)
;;; markdown-toc.el ends here
