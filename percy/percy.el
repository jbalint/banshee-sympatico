(defclass my-helm-class (helm-source) ())

(helm :sources (helm-make-source "asd" 'my-helm-class))

(helm :sources (helm-build-async-source "test2"
                 :candidates-process
                 (lambda ()
                   (start-process "wiki-pages" nil "./percy-helm-wiki-pages.sh" "arg u ment")))
      :buffer "*helm async source*")

(defvar percy-helm-output-buffer-name "*percy-helm*" "Buffer name for temporary results from process output")

(if (get-buffer percy-helm-output-buffer-name)
    (kill-buffer percy-helm-output-buffer-name))
(call-process "./percy-helm-wiki-pages.sh" nil percy-helm-output-buffer-name)

(with-current-buffer percy-helm-output-buffer-name
  (setq percy--helm-output (car (read-from-string (buffer-string)))))

(setq my-helm-source
      '((name . "Wiki Page")
        (candidates . percy--helm-output)
        (action . (lambda (c) (start-process "" nil "xdg-open" c)))))

(helm :sources '(my-helm-source))
