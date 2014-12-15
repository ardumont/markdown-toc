;;; load-markdown-toc-tests.el --- Load the namespaces for tests
;;; Commentary:
;;; Code:

(message "Launching tests!")

;; load code prod
(load-file "./markdown-toc.el")

;; Add test folder to the load path
(add-to-list 'load-path (expand-file-name "./test"))

(message "Loading tests done!")

(require 'markdown-toc)

(defun markdown-toc/test-load-namespaces! ()
  "Load the org-trello namespaces."
  (interactive)
  (mapc #'load-file '("./test/markdown-toc-tests.el")))

(markdown-toc/test-load-namespaces!)

(require 'markdown-toc-tests)

(provide 'load-markdown-toc-tests)
;;; load-markdown-toc-tests.el ends here
