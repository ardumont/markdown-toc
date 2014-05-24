;; from shell: emacs -Q --batch -l ./launch-tests.el

(load-file (expand-file-name "./load-markdown-toc-tests.el"))

(require 'load-markdown-toc-tests)

(ert-run-tests-batch-and-exit)
