;; Percy - the UI stuff
(require 'cl-lib)

(require 'percy-action)
(require 'percy-text-search-source)
(require 'percy-code-snippet-source)
(require 'percy-stardew-valley-source)

;; How to test this? Run it in Emacs without the shell scripts. Load up the
;; Helm sources, can manipulate them, and use `percy-anything'.

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

(defun percy--generate-source-candidates (desc)
  "Run the script to generate the candidates for the source"
  (if (get-buffer percy-temp-output-buffer-name)
      (kill-buffer percy-temp-output-buffer-name))
  (call-process (plist-get desc :script) nil percy-temp-output-buffer-name)
  (with-current-buffer percy-temp-output-buffer-name
    (eval (car (read-from-string (buffer-string))))))

(defun percy--update-source (desc candidates-symbol)
  "Update a source's list of candidates"
  (set candidates-symbol (percy--generate-source-candidates desc)))

(defun percy--build-source (desc-arg)
  "Build a Helm source from a Percy source descriptor"
  ;; use `lexical-let' to build the closure (capturing `desc' and
  ;; `candidates-symbol') for `update-fn'
  (lexical-let* ((desc desc-arg)
                 (name (plist-get desc :name))
                 (cached (plist-get desc :cached))
                 (candidates (if cached
                                 (let ((sym (intern (concat "percy--candidates-"
                                                            (replace-regexp-in-string "\s" "-" name)))))
                                   ;; eagerly load the candidates (could use :init but this is slightly clearer)
                                   (percy--update-source desc sym)
                                   sym)
                               (lambda () (percy--generate-source-candidates desc))))
                 (update-fn (when cached (lambda () (percy--update-source desc candidates)))))
    (helm-build-sync-source name
      :candidates candidates
      :update update-fn
      :action (plist-get desc :action)
      :nomark t
      :multimatch percy-helm-multimatch
      :fuzzy-match percy-helm-fuzzy-match)))

(setq percy--source-descriptors
      `((:name "Wiki Pages"
               :script ,(concat (getenv "BS_HOME") "/bin/percy-wiki-pages.sh")
               :action percy--xdg-open
               :cached 1)
        (:name "Jira Issues"
               :script ,(concat (getenv "BS_HOME") "/bin/percy-jira-issues.sh")
               :action percy--xdg-open
               :cached 1)
        (:name "Jira Insight Objects"
               :script ,(concat (getenv "BS_HOME") "/bin/percy-jira-insight-objects.sh")
               :action percy--xdg-open
               :cached 1)
        (:name "Bookstore"
               :script ,(concat (getenv "BS_HOME") "/bin/percy-bookstore.sh")
               :action percy--xdg-open
               :cached 1)
        (:name "Browser Tabs"
               :script ,(concat (getenv "BS_HOME") "/bin/percy-chromozol.sh")
               :cached 1
               :action percy--chromozol-open)
        (:name "Bookmarks"
               :script ,(concat (getenv "BS_HOME") "/bin/percy-bookmarks.sh")
               :action percy--xdg-open)))

;; Build the sources when the file is loaded
(setq percy--sources
      (append
       (list percy--text-search-source
             percy--code-snippet-source
             percy--stardew-valley-source)
       (cl-mapcar 'percy--build-source percy--source-descriptors)
       ))

(defun percy--close-if-client ()
  "Close the frame if it's an Emacs client"
  (if (and (boundp 'server-name)
           (equal server-name percy-emacs-server-name))
      (delete-frame)))

(defun percy-wiki-page ()
  (interactive)
  "Use Percy to open a wiki page"
  (unless (boundp 'percy--wiki-pages-candidates)
    (percy--refresh-wiki-pages-candidates))
  (helm :sources '(percy--wiki-pages-helm-source test-other-source))
  (percy--close-if-client))

(defun percy-anything ()
  (interactive)
  "Use Percy to open ANYTHING!"
  (helm :sources percy--sources)
  (percy--close-if-client))

(provide 'percy)
