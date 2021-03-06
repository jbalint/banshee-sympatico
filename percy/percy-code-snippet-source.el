;; Percy code snippet source

;; TODO : this is basic for now


(setq percy--code-snippet-source
      (helm-build-sync-source "code snippets"
        :candidates
        ;; TODO : Put a LOT more structure into these!
        '(("Stardog QueryResults.report(TextTableQueryResultWriter)" .
           "QueryResults.report(mConnection.select(query).execute(), \
new TextTableQueryResultWriter(System.out, Options.empty()));")
          ("youtube-dl" . "youtube-dl -x --audio-format mp3 ")
          ("slf4j format" . "LOGGER.info(\"Bad experience for user {}\", user);")
          ("Emacs lisp bind key globally" . "(global-set-key (kbd \"C-x C-b\") 'ibuffer)")
          ;; from https://www.jetbrains.com/help/idea/structural-search-and-replace-examples.html
          ("IntelliJ structure search" . "class $C$ {
	static String $method$(java.util.Properties $arg$);
}")
          )
;;;        :action 'kill-new
        :action 'percy--code-paste
        :nomark t
        :volatile t
        :multiline nil))

(defun percy--code-paste (code)
  "Paste code to the last focused window"
  (start-process "percy--code-paste" nil "xdotool" "type" code))

(defun percy--code-snippet--run-me ()
  (helm :sources '(percy--code-snippet-source)))

(provide 'percy-code-snippet-source)
