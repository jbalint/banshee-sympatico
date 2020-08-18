;; actions

(defun percy--echo-candidate (candidate)
  "Action for testing to print a candidate"
  (message "%s" candidate))

(defun percy--xdg-open (candidate)
  "Open a URL with `xdg-open'"
  (start-process "" nil "xdg-open" (alist-get 'url candidate)))

(defun percy--chromozol-open (candidate)
  "Activate a tab via Chromozol"
  ;; execute: bash -c 'echo tabActivate 1234.5678 > /tmp/chromozol-control.fifo'
  (let* ((tabId (alist-get 'id candidate))
         (command
          (concat "echo tabActivate " tabId " > /tmp/chromozol-control.fifo")))
    (start-process "chromozol-open" nil "bash" "-c" command)))

;; TODO: implement all this
;; (defun percy--add-jira-web-link (url)
;;   )

;; (defun percy--link-actions
;;     (helm-make-actions
;;      "Open in browser" 'percy--xdg-open
;;      "Open in Firefox" 'percy--firefox-open)


(provide 'percy-action)
