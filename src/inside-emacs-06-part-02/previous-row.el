(defun my-org-table-previous-row ()
  "Go to the previous row (same column) in the current table.
Before doing so, re-align the table if necessary."
  (interactive)
  (unless (org-at-table-hline-p)
    (org-table-maybe-eval-formula)
    (org-table-maybe-recalculate-line))
  (if (and org-table-automatic-realign
           org-table-may-need-update)
      (org-table-align))
  (let ((col (org-table-current-column)))
    (when (and (org-at-table-p)
               (not (= (org-table-current-line) 1)))
      (previous-line)
      (unless (org-at-table-hline-p)
        (org-table-goto-column col)))))

(define-key org-mode-map (kbd "M-m") 'my-org-table-previous-row)
