(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations
 (expect "   "       (markdown-toc/--symbol " " 3))
 (expect "-#--#--#-" (markdown-toc/--symbol "-#-" 3)))

(expectations
 (expect "[some markdown page title](#some-markdown-page-title)"
         (markdown-toc/--to-link "some markdown page title")))

(expectations
  (expect "- [some markdown page title](#some-markdown-page-title)
- [main title](#main-title)
    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-(recommended))
        - [Melpa-stable](#melpa-stable)
        - [Melpa (~snapshot)](#melpa-(~snapshot))
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)
- [another title](#another-title)
    - [with](#with)
    - [some](#some)
    - [heading](#heading)"
    (markdown-toc/--to-markdown-toc '((0 . "some markdown page title")
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
                                      (1 . "heading")))))

(expectations
  (expect '((0 . "Sources") (1 . "Marmalade (recommended)") (1 . "Melpa-stable"))
    (markdown-toc/--compute-toc-structure-from-level
     0
     '("Sources" ("." . 130) ("Marmalade (recommended)" . 311) ("Melpa-stable" . 552))))

  (expect '((0 . "Install") (1 . "Load org-trello") (1 . "Alternative") (2 . "Git") (2 . "Tar"))
    (markdown-toc/--compute-toc-structure-from-level
     0
     '("Install" ("." . 1184) ("Load org-trello" . 1277) ("Alternative" ("." . 1563) ("Git" . 1580) ("Tar" . 1881)))))
  (expect '((0 . "some markdown page title"))
    (markdown-toc/--compute-toc-structure-from-level
     0
     '("some markdown page title" . 1))))

(expectations
 (expect
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
  (markdown-toc/--compute-toc-structure
   '(("some markdown page title" . 1)
     ("main title"
      (#1="." . 52)
      ("Sources" (#1# . 130) ("Marmalade (recommended)" . 311) ("Melpa-stable" . 552) ("Melpa (~snapshot)" . 792))
      ("Install" (#1# . 1184) ("Load org-trello" . 1277) ("Alternative" (#1# . 1563) ("Git" . 1580) ("Tar" . 1881))))

     ("another title"
      (#1# . 2044)
      ("with" . 2061)
      ("some" . 2070)
      ("heading" . 2079))))))

(provide 'markdown-toc-tests)
;;; markdown-toc-tests.el ends here
