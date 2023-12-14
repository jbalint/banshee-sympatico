;; MUSE - knowledge graph explorer

(require 'sparql-mode)
(require 'window-layout)
(require 'org-element)
(require 'org-table)

(defvar muse:stardog-url
  "https://bs:GET_PASSWORD_FROM_PASS_COMMAND@localhost/stardog/bs/query"
  "SPARQL endpoint URL")

(defvar muse:query-get-all-types
  "select ?type (count(*) as ?count) \
from <tag:stardog:api:context:all> \
{ ?node a ?type } group by ?type"
  "Query to get all types")

(defvar muse:--gp-buffer-name "*muse:graph-pattern*")
(defvar muse:--stats-buffer-name "*muse:stats*")
(defvar muse:--types-buffer-name "*muse:types*")
(defvar muse:--results-buffer-name "*muse:results*")
(defvar muse:--temp-buffer-name "*muse:temp*")

(defun muse:variable-p (str)
  "Is `str' a variable?"
  (string-prefix-p "?" str))

;; TODO : supports URL-style IRIs only (due to Org-mode parsing)
(defun muse:single-col-table-to-list (table-ast)
  "Read the types from the table AST and return them as a list of strings"
  ;; https://orgmode.org/worg/dev/org-element-api.html
  (org-element-map table-ast 'table-row
    (lambda (row)
      ;; the types are recognized as Org-mode as URLs (links)
      (car (org-element-map row 'link
             (lambda (l) (org-element-property :raw-link l)))))))

(defun muse:curr-table ()
  "Return the current table AST"
  (car (org-element-map (org-element-parse-buffer) 'table 'identity)))

(defun muse:curr-table-rows ()
  "Return the number of non-header rows in the current table"
  (- (length (org-element-map (muse:curr-table) 'table-row 'identity)) 2))

(defun muse:query-to-curr-buffer (query)
  "Execute `query' placing results in Org table in current buffer"
  (message "Executing to %s: %s" (current-buffer) query)
  (erase-buffer)
  ;; execute query
  (sparql-execute-query query muse:stardog-url "text/csv" t)
  ;; clean up SPARQL result buffer
  (goto-char 0)
  (replace-regexp "" "")
  ;; build table
  (org-table-convert-region 1 (buffer-size) '(4))
  (org-mode)
  (flyspell-mode-off)
  ;; insert the header separate
  (goto-line 2)
  (move-beginning-of-line nil)
  (insert "|-\n")
  (backward-char)
  (org-table-next-field))

(defun muse:load-types ()
  "Load types from the server into muse:types and return the buffer with the list"
  (let ((buf (get-buffer-create muse:--types-buffer-name)))
    (with-current-buffer buf
      (muse:query-to-curr-buffer muse:query-get-all-types)
      ;; sort the table
      (org-table-sort-lines nil ?a)
      ;; load the types list
      (setq muse:types
            (muse:single-col-table-to-list (muse:curr-table))))
    buf))

(defun muse:get-type-with-completion ()
  "Show completion for types"
  (unless muse:types (error "Types not initialized"))
  (helm (helm-build-sync-source "muse:types"
          :candidates muse:types)))

(defun muse:iri-local-name (iri)
  "Return the local name of an IRI, after the last # or /"
  (cond ((string-match-p "#" iri) (replace-regexp-in-string ".*#" "" iri))
        (t (replace-regexp-in-string ".*/" "" iri))))

(defun muse:gp-add-type ()
  "Add a type to the graph pattern. If a var is on point or the subject of the current row, use that variable, otherwise add a new variable based on the type"
  (interactive)
  (let ((type (muse:get-type-with-completion)))
    (when type
      (let ((var (or (muse:gp-curr-var) (muse:var-name-from-iri type))))
        (muse:gp-add-triple-pattern (list var "a" type))))))

(defun muse:var-name-from-iri (iri)
  "Compute a variable name from an IRI. Remove 'has' prefix and adjust case"
  ;; or the class name with first-char downcase'd
  (let* ((local-name-pre (muse:iri-local-name iri))
         (local-name (if (string-prefix-p "has" local-name-pre)
                         (substring local-name-pre 3 (length local-name-pre))
                       local-name-pre))
         (first-char (substring local-name 0 1))
         (rest-chars (substring local-name 1 (length local-name))))
    (concat "?" (downcase first-char) rest-chars)))
  
(defun muse:gp-add-triple-pattern (triple-pattern)
  "Add a triple pattern to the graph pattern table"
  (message "%s" triple-pattern)
  ;; use current row if empty
  (unless (and (org-at-table-p) (not (muse:gp-curr-subject)))
    ;; else search for the var
    (goto-char 1)
    (unless (re-search-forward (car triple-pattern) nil t)
      ;; or go to the end
      (goto-char (buffer-size))
      (while (and (> (point) 0) (not (org-at-table-p)))
        (backward-char)))
    ;; then insert a new row
    (org-table-insert-row 'below))
  (insert (car triple-pattern))
  (org-table-next-field)
  (insert (cadr triple-pattern))
  (org-table-next-field)
  (insert (caddr triple-pattern))
  (org-table-previous-field)
  (org-table-previous-field))

(defun muse:gp-add-incoming ()
  "Add an incoming edge to the graph pattern"
  (interactive)
  (let ((var (muse:gp-curr-var)))
    (when var
      (let* ((prefix "select distinct ?pred from <tag:stardog:api:context:all> {\n")
             (suffix "\n} limit 500")
             (body (muse:gp-table-to-bgp (muse:curr-table)))
             (incoming-edge-pattern (format "[] ?pred %s" var))
             (query (concat prefix body " .\n" incoming-edge-pattern suffix))
             (buf (get-buffer-create muse:--temp-buffer-name)))
        (let* ((incoming-edges
                (with-current-buffer buf
                  (muse:query-to-curr-buffer query)
                  (muse:single-col-table-to-list (muse:curr-table))))
               (pred (helm (helm-build-sync-source "muse:incoming"
                             :candidates incoming-edges))))
          (when pred (muse:gp-add-triple-pattern
                      (list (muse:var-name-from-iri pred) pred var))))))))

(defun muse:gp-add-outgoing ()
  "Add an outgoing edge to the graph pattern"
  ;; TODO : lazy copy of incoming-edge code
  (interactive)
  (let ((var (muse:gp-curr-var)))
    (when var
      (let* ((prefix "select distinct ?pred from <tag:stardog:api:context:all> {\n")
             (suffix "\n} limit 500")
             (body (muse:gp-table-to-bgp (muse:curr-table)))
             (incoming-edge-pattern (format "%s ?pred []" var))
             (query (concat prefix body " .\n" incoming-edge-pattern suffix))
             (buf (get-buffer-create muse:--temp-buffer-name)))
        (let* ((incoming-edges
                (with-current-buffer buf
                  (muse:query-to-curr-buffer query)
                  (muse:single-col-table-to-list (muse:curr-table))))
               (pred (helm (helm-build-sync-source "muse:incoming"
                             :candidates incoming-edges))))
          (when pred (muse:gp-add-triple-pattern
                      (list var pred (muse:var-name-from-iri pred)))))))))

(defun muse:gp-toggle-optional ()
  "Toggle the current triple pattern OPTIONAL flag"
  (interactive)
  (error "Not implemented"))

(defun muse:gp-eval-query ()
  "Evaluate the current query"
  (interactive)
  ;; create the SPARQL query
  (let* ((prefix "select * from <tag:stardog:api:context:all> {\n")
         (suffix "\n} limit 500")
         (body (muse:gp-table-to-bgp (muse:curr-table)))
         (raw (progn
                (goto-char 1)
                (goto-char (org-table-end))
                (buffer-substring (point) (+ 1 (buffer-size)))))
         (query (concat prefix body "\n" raw suffix))
         (buf (get-buffer-create muse:--results-buffer-name)))
    (with-current-buffer buf
      (muse:query-to-curr-buffer query))))

(defun muse:gp-table-to-bgp (table-ast)
  "Convert the given table to a BGP"
  (let ((triple-patterns
         (cdr ;; skip the header row
          (org-element-map table-ast 'table-row
            (lambda (row)
              (org-element-map row 'table-cell
                (lambda (cell)
                  (let ((contents (car (org-element-contents cell))))
                    (cond
                     ((eq 'string (type-of contents)) (substring-no-properties contents))
                     ((eq 'link (org-element-type contents)) (format "<%s>" (org-element-property :raw-link contents)))
                     ;;(t (error (format "Unknown type %s" (org-element-type cell))))
                     (t nil))))))))))
    (mapconcat 'identity
               (mapcar (lambda (row) (mapconcat 'identity row " ")) triple-patterns)
               ". ")))

(defun muse:gp-curr-subject ()
  "Current subject of the graph pattern or nil of not present"
  (let ((str (substring-no-properties
              (org-table-get (org-table-current-line) 1))))
    (cond ((eq 0 (length str)) nil)
          (t str))))

(defun muse:gp-curr-cell ()
  "Current cell of the graph pattern"
  (substring-no-properties
   (org-table-get (org-table-current-line) (org-table-current-column))))

(defun muse:gp-curr-var ()
  "Current variable if at point, or else subject, if a variable"
  (let ((curr-cell (muse:gp-curr-cell))
        (curr-subj (muse:gp-curr-subject)))
    (cond
     ((not (org-at-table-p)) nil)
     ((muse:variable-p curr-cell) curr-cell)
     ((muse:variable-p curr-subj) curr-subj)
     (t nil))))

(defvar muse-graph-pattern-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c q t") 'muse:gp-add-type)
    (define-key map (kbd "C-c q i") 'muse:gp-add-incoming)
    (define-key map (kbd "C-c q o") 'muse:gp-add-outgoing)
    (define-key map (kbd "C-c q l") 'muse:gp-toggle-optional)
    (define-key map (kbd "C-c q e") 'muse:gp-eval-query)
    map)
  "Keymap for Muse Graph Pattern Mode")

(define-minor-mode muse-graph-pattern-mode
  "Minor mode for MUSE graph pattern editing"
  :keymap muse-graph-pattern-mode-map)

(defun muse:create-graph-pattern-buffer ()
  "Create initial buffer for the graph pattern"
  (let ((buf (get-buffer-create muse:--gp-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (flyspell-mode-off)
      (org-table-create "3x1")
      (org-table-next-field)
      (insert "Subject")
      (org-table-next-field)
      (insert "Predicate")
      (org-table-next-field)
      (insert "Object")
      (next-line)
      (insert "|-")
      (org-table-next-field)
      (goto-char (buffer-size))
      (insert "\n\n# Additional raw SPARQL:\n")
      (muse-graph-pattern-mode))))

(defun muse-mode ()
  "Start the MUSE mode"
  (interactive)
  (muse:load-types)
  (muse:create-graph-pattern-buffer)
  (let ((buf (get-buffer-create muse:--stats-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "TODO : populate me")))
  (let ((buf (get-buffer-create muse:--results-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "No query has been executed")))
  (setq muse:wm
        (wlf:layout
         '(- (:upper-size-ratio 0.25)
             (| (:left-size-ratio 0.5) muse:gp-buffer muse:stats-buffer)
             (- (:upper-size-ratio 0.33) muse:types-buffer muse:results-buffer))
         `((:name muse:gp-buffer :buffer ,muse:--gp-buffer-name)
           (:name muse:stats-buffer :buffer ,muse:--stats-buffer-name)
           (:name muse:types-buffer :buffer ,muse:--types-buffer-name)
           (:name muse:results-buffer :buffer ,muse:--results-buffer-name))))
  (wlf:select muse:wm 'muse:gp-buffer))

(provide 'muse-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dev stuff below

;; Window controlling
;; (wlf:show    wm 'summary)
;; (wlf:hide    wm 'summary)
;; (wlf:toggle  wm 'summary)
;; (wlf:select  wm 'summary)
;; (wlf:toggle-maximize  wm 'summary)

;; TODO : add IntelliJ-style highlighting of variables when point is at one

;; TODO : something up with org-mode and variable names with
;;        underscore in them (does it affect IRIs too?)
