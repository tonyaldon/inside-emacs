;;; part of org.el
;; find this command in the original file `lisp/org/org-table.el'
;; Emacs source repository
;; $ git clone https://git.savannah.gnu.org/git/emacs.git

(defun org-table-copy-down (n)
  "Copy the value of the current field one row below.

If the field at the cursor is empty, copy the content of the
nearest non-empty field above.  With argument N, use the Nth
non-empty field.

If the current field is not empty, it is copied down to the next
row, and the cursor is moved with it.  Therefore, repeating this
command causes the column to be filled row-by-row.

If the variable `org-table-copy-increment' is non-nil and the
field is a number, a timestamp, or is either prefixed or suffixed
with a number, it will be incremented while copying.  By default,
increment by the difference between the value in the current
field and the one in the field above, if any.  To increment using
a fixed integer, set `org-table-copy-increment' to a number.  In
the case of a timestamp, increment by days.

However, when N is 0, do not increment the field at all."
  (interactive "p")
  (org-table-check-inside-data-field)
  (let* ((beg (org-table-begin))
         (column (org-table-current-column))
         (initial-field (save-excursion
                          (let ((f (org-string-nw-p (org-table-get-field))))
                            (and f (org-trim f)))))
         field field-above next-field)
    (save-excursion
      ;; Get reference field.
      (if initial-field (setq field initial-field)
        (beginning-of-line)
        (setq field
              (catch :exit
                (while (re-search-backward org-table-dataline-regexp beg t)
                  (let ((f (org-string-nw-p (org-table-get-field column))))
                    (cond ((and (> n 1) f) (cl-decf n))
                          (f (throw :exit (org-trim f)))
                          (t nil))
                    (beginning-of-line)))
                (user-error "No non-empty field found"))))
      ;; Check if increment is appropriate, and how it should be done.
      (when (and org-table-copy-increment (/= n 0))
        ;; If increment step is not explicit, get non-empty field just
        ;; above the field being incremented to guess it.
        (unless (numberp org-table-copy-increment)
          (setq field-above
                (let ((f (unless (= beg (line-beginning-position))
                           (forward-line -1)
                           (not (org-at-table-hline-p))
                           (org-table-get-field column))))
                  (and (org-string-nw-p f)
                       (org-trim f)))))
        ;; Compute next field.
        (setq next-field (org-table--increment-field field field-above))))
    ;; Since initial field in not empty, we modify row below instead.
    ;; Skip alignment since we do it at the end of the process anyway.
    (when initial-field
      (let ((org-table-may-need-update nil)) (org-table-next-row))
      (org-table-blank-field))
    ;; Insert the new field.  NEW-FIELD may be nil if
    ;; `org-table-increment' is nil, or N = 0.  In that case, copy
    ;; FIELD.
    (insert (or next-field field))
    (org-table-maybe-recalculate-line)
    (org-table-align)))

(defun org-table-blank-field ()
  "Blank the current table field or active region."
  (interactive)
  (org-table-check-inside-data-field)
  (if (and (called-interactively-p 'any) (org-region-active-p))
      (let (org-table-clip)
        (org-table-cut-region (region-beginning) (region-end)))
    (skip-chars-backward "^|")
    (backward-char 1)
    (if (looking-at "|[^|\n]+")
        (let* ((pos (match-beginning 0))
               (match (match-string 0))
               (len (org-string-width match)))
          (replace-match (concat "|" (make-string (1- len) ?\ )))
          (goto-char (+ 2 pos))
          (substring match 1)))))
