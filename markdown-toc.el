;;; markdown-toc.el --- A simple TOC generator for markdown file
;; Copyright (C) 2014 Antoine R. Dumont

;; Author: Antoine R. Dumont
;; Maintainer: Antoine R. Dumont
;; URL: http://github.com/ardumont/markdown-toc
;; Created: 24th May 2014
;; Version: 0.0.1
;; Keywords: markdown, toc, tools,
;; Package-Requires: ((markdown-mode "2.0") (dash "2.5.0") (s "1.7.0"))

;;; Commentary:

;; Inside a markdown - M-x markdown-toc/generate-toc
;; This will compute the TOC at insert it at current position.

;; Here is a possible output:
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

;; Install - M-x package-install RET markdown-toc RET

;;; Code:

(require 's)
(require 'dash)
(require 'markdown-mode)

;; dev
;; (trace-function 'markdown-imenu-create-index)
;; (untrace-function 'markdown-imenu-create-index)

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

(defun markdown-toc/generate-toc ()
  "Called from within a markdown file, this will generate a TOC at current position."
  (interactive)
  (-> (markdown-imenu-create-index)
    markdown-toc/--compute-toc-structure
    markdown-toc/--to-markdown-toc
    insert))

(provide 'markdown-toc)
;;; markdown-toc.el ends here
