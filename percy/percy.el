;; Percy - the UI stuff

(setq helm-samewindow t)

(defvar percy-temp-output-buffer-name "*percy-temp*" "Buffer name for temporary results from process output")

(defvar percy-emacs-server-name "percy" "Server name for the Emacs server for Percy")

;; helm source for wiki pages (requires percy--wiki-pages-candidates to be initialized)
(setq percy--wiki-pages-helm-source-old
      '((name . "Wiki Pages")
        (candidates . percy--wiki-pages-candidates)
        (action . (lambda (c) (start-process "" nil "xdg-open" c)))))

(setq percy--wiki-pages-helm-source
      (helm-build-sync-source "Wiki Pages"
        :candidates percy--wiki-pages-candidates
        :action (lambda (c) (start-process "" nil "xdg-open" c))
        :fuzzy-match t))

(defun percy--refresh-wiki-pages-candidates ()
  "Refresh the set of wiki pages for helm completion"
  (if (get-buffer percy-temp-output-buffer-name)
      (kill-buffer percy-temp-output-buffer-name))
  (call-process (concat (getenv "BS_HOME") "/bin/percy-wiki-pages.sh") nil percy-temp-output-buffer-name)
  (with-current-buffer percy-temp-output-buffer-name
    (setq percy--wiki-pages-candidates (car (read-from-string (buffer-string))))))

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
  (helm :sources '(percy--wiki-pages-helm-source))
  (percy--close-if-client))
