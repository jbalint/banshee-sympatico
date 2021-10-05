;; actions

(defun percy--echo-candidate (candidate)
  "Action for testing to print a candidate"
  (message "%s" candidate))

(defun percy--xdg-open (candidate)
  "Open a URL with `xdg-open'"
  (start-process "" nil "xdg-open" (alist-get 'url candidate)))

(defun percy--firefox-open (candidate)
  "Open a URL with `firefox'"
  (start-process "" nil "firefox" (alist-get 'url candidate)))

(defun percy--xdotool-type (string)
  "Type a string in the last focused window"
  (start-process "percy--xdotool-type" nil "xdotool" "type" string))

(defun percy--xdotool-type-link-url (candidate)
  "Action to type a link's URL"
  (percy--xdotool-type (alist-get 'url candidate)))

(defun percy--xdotool-type-link-title (candidate)
  "Action to type a link's title"
  (percy--xdotool-type (alist-get 'title candidate)))

(defun percy--chromozol-open (candidate)
  "Activate a tab via Chromozol"
  ;; execute: bash -c 'echo tabActivate 1234.5678 > /tmp/chromozol-control.fifo'
  (let* ((tabId (alist-get 'id candidate))
         (command
          (concat "echo tabActivate " tabId " > /tmp/chromozol-control.fifo")))
    (start-process "chromozol-open" nil "bash" "-c" command)))

(defun percy--add-jira-web-link (candidate)
  "Link the URL to the currently open Jira issue"
  (if-let ((jira-issue-key (percy--selected-tab-jira-key)))
      (progn (if (get-buffer percy-temp-output-buffer-name)
                 (kill-buffer percy-temp-output-buffer-name))
             (let* ((url (alist-get 'url candidate))
                    (title (alist-get 'title candidate))
                    (candidate-favIconUrl (alist-get 'favIconUrl candidate))
                    (favIconUrl (cond (candidate-favIconUrl candidate-favIconUrl)
                                      ((string-match "https://localhost/" url)
                                       "https://localhost/favicon.ico"))))
               (call-process (concat (getenv "BS_HOME") "/bin/percy-add-jira-link.sh")
                             nil (get-buffer-create percy-temp-output-buffer-name) nil
                             ;; use (concat) to create an empty string - nil disallowed here
                             jira-issue-key url title (concat favIconUrl))))))

(defun percy--selected-tab-jira-key ()
  "Return the Jira key of the currently selected tab (via Chromozol) if possible"
  (let ((current-tab-url (with-current-buffer (generate-new-buffer percy-temp-output-buffer-name)
                           (call-process (concat (getenv "BS_HOME") "/bin/chromozol-current-tab.sh") nil t)
                           (let ((url (string-trim (buffer-string))))
                             (kill-buffer)
                             url))))
    (save-match-data (and (string-match "https://localhost/jira/browse/\\(.*\\)" current-tab-url)
                          (match-string 1 current-tab-url)))))

(setq percy--link-actions
      (helm-make-actions
       "Open in browser" 'percy--xdg-open
       "Open in Firefox" 'percy--firefox-open
       "Paste URL" 'percy--xdotool-type-link-url
       "Paste title" 'percy--xdotool-type-link-title
       "Link to Jira issue" 'percy--add-jira-web-link))

(setq percy--chromozol-tab-actions
      (helm-make-actions
       "Activate tab" 'percy--chromozol-open
       "Link to Jira issue" 'percy--add-jira-web-link))

(provide 'percy-action)
