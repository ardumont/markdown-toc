(require 'ert)
(require 'el-mock)

(ert-deftest markdown-toc/--symbol ()
  (should (equal "   "       (markdown-toc/--symbol " " 3)))
  (should (equal "-#--#--#-" (markdown-toc/--symbol "-#-" 3))))

(ert-deftest markdown-toc/--to-link ()
  (should (equal "[some markdown page~title (foo).](#some-markdown-pagetitle-foo)"
                 (markdown-toc/--to-link "some markdown page~title (foo)."))))

(ert-deftest markdown-toc/--to-markdown-toc ()
  (should (equal "- [some markdown page title](#some-markdown-page-title)
- [main title](#main-title)
    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)
        - [Melpa-stable](#melpa-stable)
        - [Melpa (~snapshot)](#melpa-snapshot)
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
                                                   (1 . "heading"))))))

(ert-deftest markdown-toc/--compute-toc-structure-from-level ()
  (should (equal '((0 . "Sources") (1 . "Marmalade (recommended)") (1 . "Melpa-stable"))
                 (markdown-toc/--compute-toc-structure-from-level
                  0
                  '("Sources" ("." . 130) ("Marmalade (recommended)" . 311) ("Melpa-stable" . 552)))))

  (should (equal '((0 . "Install") (1 . "Load org-trello") (1 . "Alternative") (2 . "Git") (2 . "Tar"))
                 (markdown-toc/--compute-toc-structure-from-level
                  0
                  '("Install" ("." . 1184) ("Load org-trello" . 1277) ("Alternative" ("." . 1563) ("Git" . 1580) ("Tar" . 1881))))))
  (should (equal '((0 . "some markdown page title"))
                 (markdown-toc/--compute-toc-structure-from-level
                  0
                  '("some markdown page title" . 1)))))

(ert-deftest markdown-toc/--compute-toc-structure ()
  (should (equal
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
               ("heading" . 2079)))))))

(ert-deftest markdown-toc/--compute-full-toc ()
  (should (equal
           "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->\n**Table of Contents**\n\nsome-toc\n\n<!-- markdown-toc end -->\n"
           (markdown-toc/--compute-full-toc "some-toc"))))

;; Create a new TOC
(ert-deftest markdown-toc/generate-toc--first-toc ()
  (should (equal "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [-](#-)
    - [Marmalade (recommended)](#marmalade-recommended)
    - [Melpa-stable](#melpa-stable)
    - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- markdown-toc end -->
To install **org-trello** in your emacs, you need a few steps.
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (with-temp-buffer
                   (markdown-mode)
                   (require 'markdown-toc)
                   (insert "To install **org-trello** in your emacs, you need a few steps.
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
")
                   (goto-char (point-min))
                   (markdown-toc/generate-toc)
                   (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest markdown-toc/generate-toc--replace-old-toc ()
  ;; Update an existing TOC
  (should (equal "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [-](#-)
    - [Marmalade (recommended)](#marmalade-recommended)
    - [Melpa-stable](#melpa-stable)
    - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- markdown-toc end -->
To install **org-trello** in your emacs, you need a few steps.
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (with-temp-buffer
                   (markdown-mode)
                   (require 'markdown-toc)
                   (insert "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [-](#-)
    - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- markdown-toc end -->
To install **org-trello** in your emacs, you need a few steps.
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
")
                   (goto-char (point-min))
                   (markdown-toc/generate-toc)
                   (buffer-substring-no-properties (point-min) (point-max))))))

(provide 'markdown-toc-tests)
;;; markdown-toc-tests.el ends here
