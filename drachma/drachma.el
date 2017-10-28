
(require 'widget)

(defun widget-example ()
  "Create the widgets from the Widget manual."
  (interactive)
  (with-current-buffer "*Widget Example*"
    ;;(switch-to-buffer "*Widget Example*")
    (kill-all-local-variables)
    (make-local-variable 'widget-example-repeat)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (widget-create 'menu-choice
                   :tag "Transaction type"
                   :value "Payment"
                   :help-echo "Choose me, please!"
                   :notify (lambda (widget &rest ignore)
                             (message "%s is a good choice!"
                                      (widget-value widget)))
                   '(item :tag "Transfer" :value "This")
                   '(item :tag "Adjustment" :value "This")
                   '(item :tag "Income" :value "This")
                   '(choice-item "That option")
                   '(editable-field :menu-tag "No option" "Thus option"))
    (widget-insert "\nSource:\n")
    (widget-create 'radio-button-choice
                   :value "CC2"
                   :notify (lambda (widget &rest ignore)
                             (message "You selected %s"
                                      (widget-value widget)))
                   '(item "CC1")
                   '(item "CC2")
                   '(item "Bank account")
                   '(item "Other"))
    (widget-create 'editable-field
                   ;; completions
                   :completions '("Barnes" "Frank" "Willow")
                   :size 30
                   :format "\nPayee: %v\n"
                   :notify (lambda (a &rest ignore)
                             (message "now have %s" (widget-value a)))
                   "Borders")
    (widget-create 'editable-field
                   :format "\nNote: %v\n"
                   "Bought some stuff")
    (widget-create 'editable-field
                   :size 20
                   :format "\nAmount: %v"
                   :notify (lambda (a &rest ignore)
                             (message "now have %s" (widget-value a)))
                   "67.67")
    (widget-insert " ")
    (widget-create 'editable-field
                   :size 20
                   :format "Category: %v"
                   :notify (lambda (a &rest ignore)
                             (message "now have %s" (widget-value a)))
                   "books")
    (widget-insert " ")
    (widget-create 'editable-field
                   :format "Note: %v"
                   :notify (lambda (a &rest ignore)
                             (message "now have %s" (widget-value a)))
                   "")
    (widget-insert "\n")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (if (= (length
                                     (widget-value widget-example-repeat))
                                    3)
                                 (message "Congratulation!")
                               (error "Three was the count!")))
                   "Add post")
    (widget-insert "\n")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (if (= (length
                                     (widget-value widget-example-repeat))
                                    3)
                                 (message "Congratulation!")
                               (error "Three was the count!")))
                   "Save TX")
    (widget-insert "\n")
    (use-local-map widget-keymap)
    (widget-setup)))
(widget-example)
