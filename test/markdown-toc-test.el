(require 'ert)
(require 'el-mock)
(require 'markdown-toc)
(require 'mocker)

(ert-deftest test-markdown-toc-version ()
  (let ((markdown-toc--toc-version "0.1.2"))
    (should (equal "markdown-toc version: 0.1.2" (markdown-toc-version)))))

(ert-deftest markdown-toc--symbol ()
  (should (equal "   "       (markdown-toc--symbol " " 3)))
  (should (equal "-#--#--#-" (markdown-toc--symbol "-#-" 3))))

(ert-deftest markdown-toc--to-link ()
  (should (equal "[some markdown page~title (foo).](#some-markdown-pagetitle-foo-1)"
                 (markdown-toc--to-link "some markdown page~title (foo)." 1)))
  (should (equal "[some markdown page~title (foo).](#some-markdown-pagetitle-foo)"
                 (markdown-toc--to-link "some markdown page~title (foo)." 0)))
  (should (equal "[ under_score](#under_score)"
                 (markdown-toc--to-link " under_score"))))

(ert-deftest markdown-toc--to-markdown-toc ()
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
                 (markdown-toc--to-markdown-toc '((0 . "some markdown page title")
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

(ert-deftest markdown-toc--compute-toc-structure-from-level ()
  (should (equal '((0 . "Sources") (1 . "Marmalade (recommended)") (1 . "Melpa-stable"))
                 (markdown-toc--compute-toc-structure-from-level
                  0
                  '("Sources" ("." . 130) ("Marmalade (recommended)" . 311) ("Melpa-stable" . 552)))))

  (should (equal '((0 . "Install") (1 . "Load org-trello") (1 . "Alternative") (2 . "Git") (2 . "Tar"))
                 (markdown-toc--compute-toc-structure-from-level
                  0
                  '("Install" ("." . 1184) ("Load org-trello" . 1277) ("Alternative" ("." . 1563) ("Git" . 1580) ("Tar" . 1881))))))
  (should (equal '((0 . "some markdown page title"))
                 (markdown-toc--compute-toc-structure-from-level
                  0
                  '("some markdown page title" . 1)))))

(ert-deftest markdown-toc--compute-toc-structure ()
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
           (markdown-toc--compute-toc-structure
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

(ert-deftest markdown-toc--compute-full-toc ()
  (should (equal
           "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->\n**Table of Contents**\n\nsome-toc\n\n<!-- markdown-toc end -->\n"
           (markdown-toc--compute-full-toc "some-toc"))))

(defmacro markdown-toc-with-temp-buffer-and-return-buffer-content (text body-test)
  "A `markdown-toc' test macro to ease testing.
TEXT is the content of the buffer.
BODY-TEST is the assertion to test on the buffer.
NB-LINES-FORWARD is the number of lines to get back to."
  `(with-temp-buffer
     (markdown-mode)
     (insert ,text)
     (progn
       (goto-char (point-min))
       ,body-test
       (buffer-substring-no-properties (point-min) (point-max)))))

;; Create a new TOC
(ert-deftest markdown-toc-generate-toc--first-toc ()
  (should (equal "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [something](#something)
    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)
        - [Melpa-stable](#melpa-stable)
        - [Melpa (~snapshot)](#melpa-snapshot)
        - [[配置 SPF Sender Policy Framework 记录]](#配置-spf-sender-policy-framework-记录)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- markdown-toc end -->
To install **org-trello** in your emacs, you need a few steps.
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (markdown-toc-with-temp-buffer-and-return-buffer-content
                  "To install **org-trello** in your emacs, you need a few steps.
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                  (markdown-toc-generate-toc)))))

(ert-deftest markdown-toc-generate-toc--with-duplicate-titles ()
  (should (equal "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [something](#something)
- [something](#something-1)
- [something](#something-2)
- [something](#something-3)
    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)
        - [Melpa-stable](#melpa-stable)
        - [Melpa (~snapshot)](#melpa-snapshot)
        - [[配置 SPF Sender Policy Framework 记录]](#配置-spf-sender-policy-framework-记录)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- markdown-toc end -->
To install **org-trello** in your emacs, you need a few steps.
# something
# something
# something
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (markdown-toc-with-temp-buffer-and-return-buffer-content
                  "To install **org-trello** in your emacs, you need a few steps.
# something
# something
# something
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                  (markdown-toc-generate-toc)))))

(ert-deftest markdown-toc-generate-toc--with-customs ()
  (should (equal "<!-- markdown-toc start -->
** foobar **

- [something](#something)
    - [Sources](#sources)

<!-- markdown-toc end -->
blahblah.
# something
## Sources
"
                 (let ((markdown-toc-header-toc-start "<!-- markdown-toc start -->")
                       (markdown-toc-header-toc-title "** foobar **")
                       (markdown-toc-header-toc-end "<!-- markdown-toc end -->"))
                   (markdown-toc-with-temp-buffer-and-return-buffer-content
                    "blahblah.
# something
## Sources
"
                    (markdown-toc-generate-toc))))))

(ert-deftest markdown-toc-generate-toc--first-toc-with-user-override ()
  (should (equal "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

    - [Sources](#sources)
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
# something
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
                 (let ((markdown-toc-user-toc-structure-manipulation-fn 'cdr))
                   (markdown-toc-with-temp-buffer-and-return-buffer-content
                    "To install **org-trello** in your emacs, you need a few steps.
# something
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
                    (markdown-toc-generate-toc))))))

(ert-deftest markdown-toc-generate-toc--replace-old-toc-if-already-present ()
  (should (equal "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Packages](#packages)
    - [Sources](#sources)
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
# Packages
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
                 (markdown-toc-with-temp-buffer-and-return-buffer-content
                  "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

    - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- markdown-toc end -->
To install **org-trello** in your emacs, you need a few steps.
# Packages
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
                  (markdown-toc-generate-toc)))))

(ert-deftest markdown-toc-generate-toc--replace-old-toc ()
  ;; Update an existing TOC
  (should (equal "some foo bar before

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Packages](#packages)
    - [Sources](#sources)
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
# Packages
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
                 (markdown-toc-with-temp-buffer-and-return-buffer-content
                  "some foo bar before

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

    - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- markdown-toc end -->
To install **org-trello** in your emacs, you need a few steps.
# Packages
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
                  (markdown-toc-generate-toc 'replace-old-toc)))))

(ert-deftest test-markdown-toc-generate-or-refresh-toc--with-existing-toc ()
  ;; Update an existing TOC
  (should (equal "some foo bar before

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Packages](#packages)
    - [Sources](#sources)
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
# Packages
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
                 (markdown-toc-with-temp-buffer-and-return-buffer-content
                  "some foo bar before

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

    - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- markdown-toc end -->
To install **org-trello** in your emacs, you need a few steps.
# Packages
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
                  (markdown-toc-generate-or-refresh-toc)))))

(ert-deftest test-markdown-toc-generate-or-refresh-toc--without-existing-toc ()
  (should (equal "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [something](#something)
    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)
        - [Melpa-stable](#melpa-stable)
        - [Melpa (~snapshot)](#melpa-snapshot)
        - [[配置 SPF Sender Policy Framework 记录]](#配置-spf-sender-policy-framework-记录)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- markdown-toc end -->
To install **org-trello** in your emacs, you need a few steps.
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (markdown-toc-with-temp-buffer-and-return-buffer-content
                  "To install **org-trello** in your emacs, you need a few steps.
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                  (markdown-toc-generate-or-refresh-toc)))))

(ert-deftest test-markdown-toc--refresh-toc--with-existing-toc ()
  ;; Update an existing TOC
  (should (equal "some foo bar before

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Packages](#packages)
    - [Sources](#sources)
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
# Packages
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
                 (markdown-toc-with-temp-buffer-and-return-buffer-content
                  "some foo bar before

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

    - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- markdown-toc end -->
To install **org-trello** in your emacs, you need a few steps.
# Packages
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
                  (markdown-toc-refresh-toc)))))

(ert-deftest test-markdown-toc-refresh-toc--without-existing-toc ()
  (should (equal "To install **org-trello** in your emacs, you need a few steps.
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                 (markdown-toc-with-temp-buffer-and-return-buffer-content
                  "To install **org-trello** in your emacs, you need a few steps.
# something
## Sources
If not already configured, you need to prepare emacs to work with marmalade or melpa.
For this, you need to install a snippet of code in your emacs configuration file.
### Marmalade (recommended)
### Melpa-stable
### Melpa (~snapshot)
### [配置 SPF Sender Policy Framework 记录]
## Install
### Load org-trello
### Alternative
#### Git
#### Tar
"
                  (markdown-toc-refresh-toc)))))

(ert-deftest test-markdown-toc-delete-toc ()
  (should (equal "To install **org-trello** in your emacs, you need a few steps.
# Packages
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
                 (markdown-toc-with-temp-buffer-and-return-buffer-content
                   "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

    - [Melpa (~snapshot)](#melpa-snapshot)
    - [Install](#install)
        - [Load org-trello](#load-org-trello)
        - [Alternative](#alternative)
            - [Git](#git)
            - [Tar](#tar)

<!-- markdown-toc end -->
To install **org-trello** in your emacs, you need a few steps.
# Packages
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
                  (markdown-toc-delete-toc)))))

(ert-deftest test-markdown-toc-log-msg ()
  (should (string= "markdown-toc - hello dude"
                   (mocker-let ((message (str &rest args)
                                         ((:input '("markdown-toc - hello %s" "dude") :output "markdown-toc - hello dude"))))
                     (markdown-toc-log-msg '("hello %s" "dude"))))))

(ert-deftest test-markdown-toc--bug-report ()
  (should (string=
           "Please:
- Describe your problem with clarity and conciceness (cf. https://www.gnu.org/software/emacs/manual/html_node/emacs/Understanding-Bug-Reporting.html)
- Explicit your installation choice (melpa, marmalade, el-get, tarball, git clone...).
- Report the following message trace inside your issue.

System information:
- system-type: system-type
- locale-coding-system: locale-coding-system
- emacs-version: emacs-version
- markdown-mode path: /path/to/markdown-mode
- markdown-toc version: markdown-toc-version
- markdown-toc path: /path/to/markdown-toc"
           (let ((system-type "system-type")
                 (locale-coding-system "locale-coding-system")
                 (markdown-toc--toc-version "markdown-toc-version")
                 (request-backend "curl"))
             (mocker-let ((emacs-version ()
                                         ((:input nil :output "emacs-version")))
                          (find-library-name (lib)
                                             ((:input '("markdown-mode") :output "/path/to/markdown-mode")
                                              (:input '("markdown-toc") :output "/path/to/markdown-toc"))))
               (markdown-toc--bug-report))))))

(ert-deftest test-markdown-toc-bug-report ()
  (should (equal :res
                 (mocker-let ((browse-url (url)
                                          ((:input '("https://github.com/ardumont/markdown-toc/issues/new")
                                                   :output :opened)))
                              (markdown-toc--bug-report ()
                                                        ((:input nil :output :message)))
                              (markdown-toc-log-msg (args)
                                                    ((:input '((:message)) :output :res))))
                   (markdown-toc-bug-report 'browse))))
  (should (equal :res2
                 (mocker-let ((markdown-toc--bug-report ()
                                                        ((:input nil :output :message2)))
                              (markdown-toc-log-msg (args)
                                                    ((:input '((:message2)) :output :res2))))
                   (markdown-toc-bug-report)))))

(ert-deftest markdown-toc--read-title-out-of-link ()
  (should (string= "this is the title"
                   (markdown-toc--read-title-out-of-link "  - [this is the title](#this-is-the-link)   ")))
  (should (string= "another title"
                   (markdown-toc--read-title-out-of-link "  - [another title](#this-is-the-link)
with multiple line
should not matter "))))

(ert-deftest markdown-toc--title-level ()
  (should (eq 1
              (markdown-toc--title-level "- [this is the title](#this-is-the-link)")))
  (should (eq 4
              (let ((markdown-toc-indentation-space 4))
                (markdown-toc--title-level "            - [this is the title](#this-is-the-link)"))))
  (should (eq 2
              (let ((markdown-toc-indentation-space 2))
                (markdown-toc--title-level "  - [another title](#this-is-the-link)
with multiple line
should not matter "))))
  (should (eq 2
              (let ((markdown-toc-indentation-space 3))
                (markdown-toc--title-level "   - [another title](#this-is-the-link)
with multiple line
should not matter "))))
  ;; no - as prefix so considered not a title
  (should-not (markdown-toc--title-level "[this is the title](#this-is-the-link)"))
  ;; prefixed with a dash but misaligned, title should be indented with a
  ;; multiple of `markdown-toc-indentation-space` blank spaces
  (should-not (markdown-toc--title-level " - [title](#this-is-the-link)")))

(ert-deftest markdown-toc-follow-link-at-point()
  "Follow a correct toc link should follow to the title"
  (should (string= "## Sources"
                   (with-temp-buffer
                     (insert "- [some markdown page title](#some-markdown-page-title)
- [main title](#main-title)
    - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)

# main title
## Sources
### marmalade
...
")
                     (search-backward "- [Sources]")
                     (call-interactively 'markdown-toc-follow-link-at-point)
                     (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(ert-deftest markdown-toc-follow-link-at-point-failures()
  "Follow a misindented toc link should do nothing"
  (should
   ;; not move
   (string= "   - [Sources](#sources)  <- misindented 3 instead of 4 here"
            (with-temp-buffer
              (insert "- [some markdown page title](#some-markdown-page-title)
- [main title](#main-title)
   - [Sources](#sources)  <- misindented 3 instead of 4 here

# main title
## Sources
...
")
              (search-backward "- [Sources]")
              (call-interactively 'markdown-toc-follow-link-at-point)
              (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))

  (should
   ;; not move as well because
   (string= "not a title"
            (with-temp-buffer
              (insert "- [some markdown page title](#some-markdown-page-title)
- [main title](#main-title)
   - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)

# main title
## Sources
### marmalade
not a title
...
")
              (search-backward "not a title")
              (call-interactively 'markdown-toc-follow-link-at-point)
              (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(provide 'markdown-toc-tests)
;;; markdown-toc-tests.el ends here
