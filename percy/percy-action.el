;; actions

(defun percy--xdg-open (url)
  "Open a URL with `xdg-open'"
  (start-process "" nil "xdg-open" url))

(defun percy--chromozol-open (tabId)
  "Activate a tab via Chromozol"
  ;; execute: bash -c 'echo tabActivate 1234.5678 > /tmp/chromozol-control.fifo'
  (let ((command
         (concat "echo tabActivate " tabId " > /tmp/chromozol-control.fifo")))
    (start-process "chromozol-open" nil "bash" "-c" command)))

(provide 'percy-action)
