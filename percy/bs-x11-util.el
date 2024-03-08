
;; Make sure we have `xprop`.
(unless (string= "0" (s-trim (shell-command-to-string
                              "which xprop >& /dev/null ; echo $?")))
  (error "xprop not available"))

(defun x11-window-list ()
  "Retrieve the list of windows as a set of window id values"
  (declare (side-effect-free t))
  (s-split ", "
           (s-trim
            (s-chop-prefix "_NET_CLIENT_LIST(WINDOW): window id # "
                           (shell-command-to-string
                            "xprop -root 32ic _NET_CLIENT_LIST")))))

(defun x11-window-description (id)
  "Retrieve the string description of a window given by it's id"
  ;; TEST: (win-description (car (win-list)))
  (declare (side-effect-free t))
  (let* ((xprop-output (s-trim
                        (shell-command-to-string
                         (format "xprop -id %s WM_NAME WM_CLASS" id))))
         (extracted (s-replace-regexp "WM_NAME(.*) = \"\\(.*\\)\"" "\\1" xprop-output)))
    extracted))

(defun bspwm-focus-window (id)
  (shell-command (format "bspc node -f %s" id)))

(provide 'bs-x11-util)
