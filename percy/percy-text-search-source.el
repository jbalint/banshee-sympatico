;; Percy text search source

(require 'request)
(require 'json)
(require 'helm)
(require 'percy-action)

;; (setq request-log-level 'trace)

(defconst percy--text-search-query
  (with-temp-buffer
    (insert-file-contents (concat (getenv "BS_HOME") "/percy/percy-text-search.rq"))
    (buffer-string))
  "The SPARQL query to perform the text search")

(defvar percy--text-search-candidates nil
  "Holds the candidates which are computed out-of-band")

(defun percy--sparql-json-value (v e)
  "Retrieve the value of the variable `v' from the SPARQL bindings `e'."
  (cdr (assoc 'value (assoc v e))))

(defun percy--text-to-display (x)
  "Compute the display value of a text search result"
  (let* ((header (concat "[[" (percy--sparql-json-value 'title x) "]] - "))
         ;; apply the face to the highlighted matches
         (display-text
          (with-temp-buffer
            (insert (replace-regexp-in-string
                     ;; change NEWLINE to \n to collapse search results to single lines
                     "NEWLINE" " " (percy--sparql-json-value 'h x)))
            (goto-char 0)
            (while (re-search-forward "<b>\\(.*?\\)</b>" nil t)
              (replace-match "\\1")
              (helm-add-face-text-properties
               (match-beginning 0) (match-end 0) 'helm-visible-mark))
            (buffer-string))))
    (concat header display-text)))

(defun percy--text-search-open-url (candidate)
  (percy--xdg-open `((url . ,(percy--sparql-json-value 'url candidate)))))

;; perform a SPARQL query, returning the result as a JSON object, as parsed by json.el
(defun percy--text-search-fetch-candidates (text-query)
  (when (< 0 (length text-query))
    "Perform text query against DB and return list of candidates"
    (let* ((resp (request
                  "https://localhost/stardog/bs/query"
                  :sync t
                  :params `(("query" . ,percy--text-search-query)
                            ("$textQuery" . ,(format "'%s'" text-query)))
                  :parser 'json-read
                  :headers '(("Accept" . "application/sparql-results+json")
                             ;; TODO: use env for credentials
                             ("Authorization" . "Basic YWRtaW46YWRtaW4="))))
           ;; this returns a vector
           (bindings (cdr (cadr (cadr (request-response-data resp))))))
      (mapcar (lambda (el) (cons (percy--text-to-display el) el)) bindings))))

(setq percy--text-search-source
      (helm-build-sync-source "text search"
        :init (lambda () (setq percy--text-search-candidates nil))
        :candidates (lambda () (eval 'percy--text-search-candidates))
        :action 'percy--text-search-open-url
        :nomark t
        :volatile t
        :multiline t))

(defun percy--text-search-update ()
  "Update the list of candidates. Should be called while Helm is running"
  (interactive)
  (setq percy--text-search-candidates (percy--text-search-fetch-candidates helm-pattern))
  (helm-update))

(define-key helm-map (kbd "C-c C-s") 'percy--text-search-update)

;; this doesn't work in the (helm) call and doesn't work per source
;; because the candidates is initially empty and we can't "select" The
;; source
;; (defvar percy--text-search-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map helm-map)
;;     (define-key map (kbd "C-c C-s") 'percy--text-search-update)
;;     map) "Keymap to refresh Percy text search source")

(defun percy--text-search--run-me ()
  (helm :sources '(percy--text-search-source)))

(provide 'percy-text-search-source)

