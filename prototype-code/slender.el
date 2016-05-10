
;; http://oscaf.sourceforge.net/ncal.html
;; http://oscaf.sourceforge.net/tmo.html

(define-derived-mode slender-mode
  org-mode "Slender"
  "Major mode for Slender task manager")

(defun sln--get-tasks ()
  '((sln:brushCat
	 (rdf:type . tmo:Task)
	 (rdf:type . ncal:Todo)
	 (tmo:taskName . "Brush the cat")
	 (ncal:freq . ncal:daily))
	(sln:cleanStudio
	 (rdf:type . tmo:Task)
	 (rdf:type . ncal:Todo)
	 (tmo:taskName . "Clean the studio")
	 (ncal:freq . ncal:daily))
	(sln:useEmacsHydra
	 (rdf:type . tmo:Task)
	 (rdf:type . ncal:Todo)
	 ;; this is an anonymous object
	 (ncal:due . ((ncal:date . (meera:date "2015-08-12"))))
	 (tmo:taskName . "Use Hydra")
	 (tmo:taskDescription . "Use Hydra from https://github.com/abo-abo/hydra and check out his video demo of window navigation"))))

(defun sln--longest-name (tasks max)
  (let* ((get-task-name-len (lambda (task) (length (cdr (assoc 'tmo:taskName task)))))
		 (longest-name (apply 'max (mapcar get-task-name-len tasks))))
	(min max longest-name)))

(defun sln--print-tasks (tasks)
  (let* ((name-col-len (sln--longest-name tasks 50))
		 (fmt (concat "| %" (number-to-string name-col-len) "s |\n")))
	(insert (make-string (+ 4 name-col-len) ?-) "\n")
	(mapcar (lambda (task) (insert (format fmt (cdr (assoc 'tmo:taskName task))))) tasks)))

(defun sln--save-task ()
  (interactive)
  (let* ((tree (org-element-parse-buffer))
		 (name (car (org-element-map tree 'headline
					  (lambda (hl) (car (org-element-property :title hl))))))
		 (deadline-ts (car (org-element-map tree 'planning
							 (lambda (pl) (org-element-property :deadline pl)))))
		 (scheduled-ts (car (org-element-map tree 'planning
							  (lambda (pl) (org-element-property :scheduled pl)))))
		 ;;(desc (mapconcat 'identity (org-element-map extree 'paragraph
		 (desc (mapconcat (lambda (a) (concat "- " a)) (org-element-map extree 'paragraph
									  (lambda (p) (car (org-element-contents (org-element-normalize-contents p))))) "\n"))
		 (deadline-str (if deadline-ts (format "%04d\\-%02d\\-%02d"
											   (org-element-property :year-start deadline-ts)
											   (org-element-property :month-start deadline-ts)
											   (org-element-property :day-start deadline-ts))
						 nil))
		 (scheduled-str (if scheduled-ts (format "%04d-%02-%02d"
												 (org-element-property :year-start scheduled-ts)
												 (org-element-property :month-start scheduled-ts)
												 (org-element-property :day-start scheduled-ts))))
		 (new-task `(TODO:NAME
					 (rdf:type . tmo:Task)
					 (rdf:type . ncal:Todo)
					 ;; TODO conditionally add-to-list these props (w/ append=t)
					 (tmo:dueDate . (meera:date ,deadline-str))
					 (ncal:dtstart . ((ncal:date . (meera:date ,scheduled-str))))
					 (tmo:taskname . ,name)
					 (tmo:taskDescription . ,desc))))
	(message "%s" new-task)
	))
  ;;(kill-buffer))

(defun slender-new-task ()
  (interactive)
  (switch-to-buffer "*slender-task*")
  (erase-buffer)
  (insert "* ")
  (org-mode)
  (local-set-key (kbd "C-c C-c") 'sln--save-task)
  )

(defun slender ()
  "Run Slender"
  (interactive)
  (switch-to-buffer "*slender*")
  (erase-buffer)
  (sln--print-tasks (sln--get-tasks))
  )
