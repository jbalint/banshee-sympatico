;; Percy - the UI stuff

;; copied from xml.el's `xml-escape-string'
(defun xml-unescape-string (string)
  (with-temp-buffer
    (insert string)
    (dolist (substitution '(("&amp;" . "&")
			    ("&lt;" . "<")
			    ("&gt;" . ">")
			    ("&apos;" . "'")
			    ("&quot;" . "\"")))
      (goto-char (point-min))
      (while (search-forward (car substitution) nil t)
	(replace-match (cdr substitution) t t nil)))
    (buffer-string)))

;; (makunbound 'percy--jira-issues-candidates)
;; (makunbound 'percy--wiki-pages-candidates)

(setq helm-samewindow t)

(defvar percy-temp-output-buffer-name "*percy-temp*" "Buffer name for temporary results from process output")

(defvar percy-emacs-server-name "percy" "Server name for the Emacs server for Percy")

(defvar percy-helm-fuzzy-match nil)
(defvar percy-helm-multimatch t)

;; helm source for wiki pages (requires percy--wiki-pages-candidates to be initialized)
;; check helm-source.el for details (describe-function 'helm-build-sync-source)
;; 2 - template
(setq percy--wiki-pages-helm-source
      (helm-build-sync-source "Wiki Pages"
        :candidates 'percy--wiki-pages-candidates
        :action (lambda (c) (start-process "" nil "xdg-open" c))
        :nomark t
        :multimatch percy-helm-multimatch
        :update (lambda () (message "UPDATING"))
        :fuzzy-match percy-helm-fuzzy-match))
;; 2 - template
(setq percy--jira-issues-helm-source
      (helm-build-sync-source "Jira Issues"
        :candidates 'percy--jira-issues-candidates
        :action (lambda (c) (start-process "" nil "xdg-open" c))
        :nomark t
        :multimatch percy-helm-multimatch
        :fuzzy-match percy-helm-fuzzy-match))
(setq percy--jira-search-helm-source
      (helm-build-sync-source "Jira Search"
        :candidates 'percy--jira-search-candidates
        :action (lambda (c) (start-process "" nil "xdg-open" c))
        :nomark t
        :multimatch percy-helm-multimatch
        :fuzzy-match percy-helm-fuzzy-match))

;; 1 - template / how to generalize this over sources?
(defun percy--refresh-wiki-pages-candidates ()
  "Refresh the set of wiki pages for helm completion"
  (if (get-buffer percy-temp-output-buffer-name)
      (kill-buffer percy-temp-output-buffer-name))
  (call-process (concat (getenv "BS_HOME") "/bin/percy-wiki-pages.sh") nil percy-temp-output-buffer-name)
  (with-current-buffer percy-temp-output-buffer-name
    (setq percy--wiki-pages-candidates (car (read-from-string (buffer-string))))))
;; 1 - template / how to generalize this over sources?
;; DIFFERENCE: using an eval after reading the string to get the `xml-unescape-string' to be invoked
(defun percy--refresh-jira-issues-candidates ()
  "Refresh the set of Jira issues for helm completion"
  (if (get-buffer percy-temp-output-buffer-name)
      (kill-buffer percy-temp-output-buffer-name))
  (call-process (concat (getenv "BS_HOME") "/bin/percy-jira-issues.sh") nil percy-temp-output-buffer-name)
  (with-current-buffer percy-temp-output-buffer-name
    (setq percy--jira-issues-candidates (eval (car (read-from-string (buffer-string)))))))
;; TODO temp
(defun percy--refresh-jira-search-candidates ()
  "Refresh the set of wiki pages for helm completion"
  (if (get-buffer percy-temp-output-buffer-name)
      (kill-buffer percy-temp-output-buffer-name))
  (call-process (concat (getenv "BS_HOME") "/bin/percy-jira-search.sh") nil percy-temp-output-buffer-name)
  (with-current-buffer percy-temp-output-buffer-name
    (setq percy--jira-search-candidates (eval (car (read-from-string (buffer-string)))))))

(defun percy--close-if-client ()
  "Close the frame if it's an Emacs client"
  (if (and (boundp 'server-name)
           (equals server-name percy-emacs-server-name))
      (delete-frame)))

(defun percy-wiki-page ()
  (interactive)
  "Use Percy to open a wiki page"
  (unless (boundp 'percy--wiki-pages-candidates)
    (percy--refresh-wiki-pages-candidates))
  (helm :sources '(percy--wiki-pages-helm-source test-other-source))
  (percy--close-if-client))

;; TODO - need a way to manually force a refresh and a timer to do it periodically
(defun percy-anything ()
  (interactive)
  "Use Percy to open ANYTHING!"
  (unless (boundp 'percy--wiki-pages-candidates)
    (percy--refresh-wiki-pages-candidates))
  (unless (boundp 'percy--jira-issues-candidates)
    (percy--refresh-jira-issues-candidates))
  (helm :sources '(percy--wiki-pages-helm-source
                   percy--jira-issues-helm-source
                   ;percy--jira-search-helm-source
                   ))
  (percy--close-if-client))

(defun percy-reset ()
  "Clear cached candidates"
  (makunbound 'percy--wiki-pages-candidates)
  (makunbound 'percy--jira-issues-candidates))
