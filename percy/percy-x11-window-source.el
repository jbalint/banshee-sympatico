(require 's)
(require 'bs-x11-util)

(defun percy--x11-window-list-as-desc-and-id-pairs ()
  ""
  (cl-mapcar (lambda (id) `(,(x11-window-description id) . ,id)) (x11-window-list)))

(setq percy--x11-window-source
      (helm-build-sync-source "X11 Windows"
        ;; Use a function rather than an _invocation_ of the function
        ;; so it's called every time resulting in an up-to-date list
        ;; of windows.
        :candidates 'percy--x11-window-list-as-desc-and-id-pairs
        :action 'bspwm-focus-window
        :nomark t
        :volatile t
        :multiline nil))

(defun ---percy--x11-window--run-me ()
  "Utility function to test this source"
  (helm :sources '(percy--x11-window-source)))

(provide 'percy-x11-window-source)
