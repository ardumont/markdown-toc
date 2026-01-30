;;; markdown-toc-pandoc.el --- Generates TOC compatible with pandoc -*- lexical-binding: t; -*-

;;; Code:

(require 's)
(require 'dash)

(require 'markdown-toc-common)

(defun markdown-toc--protected-data-push (text prefix index protected-data)
  "Tag TEXT by PREFIX and INDEX and push into PROTECTED-DATA.
PROTECTED-DATA should be (list data-alist)."
  (let ((tag (format "\uE000%s%d\uE001" prefix index)))
    (setcar protected-data (cons (cons tag text) (car protected-data)))
    tag))

(defun markdown-toc--protected-data-restore (text protected-data)
  "Restore all tags from PROTECTED-DATA to their original content.
PROTECTED-DATA should be (list data-alist)."
  (let ((data (car protected-data)))
    (if (null data)
        text
      (let ((rgx (regexp-opt (mapcar #'car data))))
        (save-match-data
          (replace-regexp-in-string
           rgx
           (lambda (matched)
             (or (cdr (assoc matched data))
                 matched))
           text t t))))))

(defun markdown-toc--protect-escaped-and-code (text protected-data)
  "Protect backslash-escaped characters and inline code spans.
Handles them in order of appearance to respect escaping rules.
PROTECTED-DATA should be (list data-alist)."
  (let ((esc-index 0)
        (code-index 0))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))

      ;; Combined regex: escaped char OR backtick sequence
      (let ((combined-rx (rx (or (seq "\\" (group-n 1 anything))
                                 (group-n 2 (+ "`"))))))
        (while (re-search-forward combined-rx nil t)
          (cond
           ;; Case 1: Escaped character (group 1 matched)
           ((match-string 1)
            (let* ((escaped-char (match-string 1))
                   (start-pos (match-beginning 0))
                   (end-pos (match-end 0))
                   (tag (markdown-toc--protected-data-push
                         escaped-char "ESC" (cl-incf esc-index)
                         protected-data)))
              (replace-region-contents start-pos end-pos
                                       (lambda () tag))))

           ;; Case 2: Code span delimiter (group 2 matched)
           ((match-string 2)
            (let* ((delim (match-string 2))
                   (len (length delim))
                   (start-pos (match-beginning 0))
                   ;; Look for closing delimiter (not escaped, same length)
                   (end-rx (concat (regexp-quote delim) "\\([^`]\\|$\\)")))

              (if (re-search-forward end-rx nil t)
                  (let* ((end-pos (- (point)
                                     (if (= (match-beginning 1)
                                            (point-max))
                                         0 1)))
                         (content (string-trim
                                   (replace-regexp-in-string
                                    "\n" " "
                                    (buffer-substring-no-properties
                                     (+ start-pos len)
                                     (- end-pos len)))))
                         (tag (markdown-toc--protected-data-push
                               content "CODE" (cl-incf code-index)
                               protected-data)))
                    (replace-region-contents start-pos end-pos
                                             (lambda () tag)))
                ;; No closing delimiter found, move past opening
                (goto-char (1+ start-pos))))))))
      (buffer-string))))

(defun markdown-toc--basic-smart-punctuation (text)
  "Convert ASCII sequences into punctuation marks.
Note: single/double quotes or apostrophes are not supported."
  (->> text
       (replace-regexp-in-string (rx "...") "…")
       (replace-regexp-in-string (rx "---") "—")
       (replace-regexp-in-string (rx "--")  "–")))

(defun markdown-toc--strip-md-format-iterative (text)
  "Iteratively strip formatting tags until no changes remain.
Note: underscored bolds or italics and autolinks are not supported:
all underscore remain as-is while autolinks are treated as html tags."
  (cl-loop for res = text then
           (cond
            ;; 1. HTML Tags
            ((string-match (rx "<" (+? anything) ">") res)
             (replace-match "" t t res))

            ;; 2. Links and Images
            ((string-match (rx (opt "!") "[" (group (*? anything)) "]"
                               (or (seq "(" (*? anything) ")")
                                   (seq "[" (*? anything) "]"))) res)
             (replace-match (match-string 1 res) t t res))

            ;; 3. Bold-Italic
            ((string-match (rx (group-n 1 (or "***"))
                               (group-n 2 (+? (not (any "\n"))))
                               (backref 1)) res)
             (replace-match (match-string 2 res) t t res))

            ;; 4. Bold or Strikethrough
            ((string-match (rx (group-n 1 (or "**" "~~"))
                               (group-n 2 (+? (not (any "\n"))))
                               (backref 1)) res)
             (replace-match (match-string 2 res) t t res))

            ;; 5. Italic
            ((string-match (rx (group-n 1 "*")
                               (group-n 2 (+? (not (any "*\n"))))
                               (backref 1)) res)
             (replace-match (match-string 2 res) t t res))

            ;; 6. Superscript
            ((string-match (rx "^"
                               (group-n 1
                                 (not space)
                                 (opt (*? (not (any "\n")))
                                      (not space)))
                               "^") res)
             (replace-match (match-string 1 res) t t res))

            ;; 7. Subscript
            ((string-match (rx "~"
                               (group-n 1
                                 (not space)
                                 (opt (*? (not (any "\n")))
                                      (not space)))
                               "~") res)
             (replace-match (match-string 1 res) t t res))

            ;; No more matches: Exit loop
            (t (cl-return res)))))

(defun markdown-toc--strip-markdown-formatting (text)
  "Pipeline to strip common Markdown formatting from TEXT."
  (let ((protected-data (list nil)))
    (-> text
        (markdown-toc--protect-escaped-and-code protected-data)
        (markdown-toc--basic-smart-punctuation)
        (markdown-toc--strip-md-format-iterative)
        (markdown-toc--protected-data-restore protected-data))))

(defun markdown-toc--remove-variation-selectors (text)
  "Remove emoji variation selectors from TEXT."
  (replace-regexp-in-string "[\uFE0E\uFE0F]" "" text))

(defun markdown-toc--space-to-dash (text)
  (mapconcat #'identity (split-string text "[[:space:]]+" t) "-"))

(defun markdown-toc--to-slug (title)
  "Generate slug from TITLE. Strips common markdown formatting.
Note: Unicode superscripts or subscripts are not supported."
  (let* ((case-fold-search nil)
         (slug
          (->> title
               (markdown-toc--remove-variation-selectors)
               (markdown-toc--strip-markdown-formatting)
               (downcase)
               (replace-regexp-in-string "[^[:alnum:][:space:]_.-]" "")
               (markdown-toc--space-to-dash)
               (replace-regexp-in-string "[[:space:]]+" "-")
               (replace-regexp-in-string "\\`[^[:alpha:]]+" ""))))
    (if (string-empty-p slug) "section" slug)))

(defun markdown-toc--get-unique-slug (base-slug used-slugs)
  "Find unique variant of BASE-SLUG avoiding USED-SLUGS,
where USED-SLUGS is a hash table. Replica of Pandoc uniqueIdent."
  (cond
   ((not (gethash base-slug used-slugs))
    base-slug)
   ((cl-loop for n from 1 to 60000
             for candidate = (format "%s-%d" base-slug n)
             unless (gethash candidate used-slugs)
             return candidate))
   (t base-slug)))

(defun markdown-toc--generate-unique-slugs (toc-structure)
  "Process TOC-STRUCTURE, generating unique slugs.
Input: ((level . title) ... ); Output: ((level title slug) ... )"
  (let ((used-slugs (make-hash-table :test 'equal))
        (result nil))
    (dolist (item toc-structure)
      (let* ((level (car item))
             (title (cdr item))
             (base-slug (markdown-toc--to-slug title))
             (unique-slug (markdown-toc--get-unique-slug
                           base-slug used-slugs)))
        (puthash unique-slug t used-slugs)
        (push (list level title unique-slug) result)))
    (nreverse result)))

(defun markdown-toc--to-markdown-toc-pandoc (level-title-toc-list)
  "Given LEVEL-TITLE-TOC-LIST, return a TOC string.
This correctly handles most cases but not perfectly aligned with pandoc."
  (->> level-title-toc-list
       markdown-toc--generate-unique-slugs
       (--map (let ((nb-spaces (* markdown-toc-indentation-space (car it)))
                    (title     (cadr it))
                    (slug      (caddr it)))
                (format "%s%s [%s](#%s)"
                        (markdown-toc--symbol " " nb-spaces)
                        markdown-toc-list-item-marker
                        (string-trim title)
                        slug)))
       (s-join "\n")))

(provide 'markdown-toc-pandoc)
;;; markdown-toc-pandoc.el ends here
