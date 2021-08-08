;;; my-org-meta-return

(defun my-org-meta-return (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item' or
`org-table-wrap-region', depending on context."
  (interactive "P")
  (org-check-before-invisible-edit 'insert)
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (if (org-at-table-p)
          (org-table-wrap-region arg)
        (call-interactively
         (cond (arg #'org-insert-heading)
               ((org-in-item-p) #'org-insert-item)
               (t #'org-insert-heading))))))

(define-key org-mode-map (kbd "<M-return>") 'my-org-meta-return)
