;;; markdown-toc.el --- A simple TOC generator for markdown file
;; Copyright (C) 2014 Antoine R. Dumont

;; Author: Antoine R. Dumont
;; Maintainer: Antoine R. Dumont
;; URL: http://github.com/ardumont/markdown-toc
;; Created: 24th May 2014
;; Version: 0.0.1
;; Keywords: lisp, tools, markdown, toc
;; Package-Requires: ((markdown-mode "2.0") (dash "2.5.0") (s "1.7.0"))

;;; Commentary:

;;; Code:

(require 'markdown-mode)

;; dev
;; (trace-function 'markdown-imenu-create-index)
;; (untrace-function 'markdown-imenu-create-index)

(defun markdown-toc/--compute-toc (imenu-index)
  "Given a IMENU-INDEX, compute the TOC structure."
  (let ((fst  (car imenu-index))
        (tail (cdr imenu-index)))
    (cond ((integerp (cdr fst)) (cons (car fst) (markdown-toc/--compute-toc tail)))
    )))

;; Initial parsing
'(("some markdown page title" . 1)
  ("main title"
  (#1="." . 52)
  ("Sources" (#1# . 130) ("Marmalade (recommended)" . 311) ("Melpa-stable" . 552) ("Melpa (~snapshot)" . 792))
  ("Install" (#1# . 1184) ("Load org-trello" . 1277) ("Alternative" (#1# . 1563) ("Git" . 1580) ("Tar" . 1881))))

 ("another title"
  (#1# . 2044)
  ("with" . 2061)
  ("some" . 2070)
  ("heading" . 2079)))

(defun markdown-toc/--symbol (sym n)
  "Compute the repetition of a symbol SYM N times as a string."
  (--> n
    (-repeat it sym)
    (s-join "" it)))

(defun markdown-toc/--to-markdown-toc (level-title-toc-list)
  "Given LEVEL-TITLE-TOC-LIST, a list of pair level, title, return a TOC string."
  (->> level-title-toc-list
    (--map (let ((nb-spaces (* 8 (car it)))
                 (title     (cdr it)))
             (format "%s- %s" (markdown-toc/--symbol " " nb-spaces) (markdown-toc/--to-link title))))
    (s-join "\n")))

(defun markdown-toc/--to-link (title)
  "Given a TITLE, return the markdown link associated."
  (format "[%s](#%s)" title (replace-regexp-in-string " " "-" (downcase title))))

(defun markdown-toc/generate-toc ()
  "Called from within a markdown file, this will generate a TOC at current position."
  (interactive)
  (markdown-imenu-create-index))

(provide 'markdown-toc)
;;; markdown-toc.el ends here
