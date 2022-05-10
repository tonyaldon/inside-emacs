;;; part of org.el
;; find this command in the original file `lisp/org/org-table.el'
;; Emacs source repository
;; $ git clone https://git.savannah.gnu.org/git/emacs.git

(defun org-table-next-row ()
  "Go to the next row (same column) in the current table.
Before doing so, re-align the table if necessary."
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
           org-table-may-need-update)
      (org-table-align))
  (let ((col (org-table-current-column)))
    (beginning-of-line 2)
    (unless (bolp) (insert "\n"))  ;missing newline at eob
    (when (or (not (org-at-table-p))
              (org-at-table-hline-p))
      (beginning-of-line 0)
      (org-table-insert-row 'below))
    (org-table-goto-column col)
    (skip-chars-backward "^|\n\r")
    (when (looking-at " ") (forward-char))))

(defun org-table-current-column ()
  "Return current column number."
  (interactive)
  (save-excursion
    (let ((pos (point)))
      (beginning-of-line)
      (if (not (search-forward "|" pos t)) 0
        (let ((column 1)
              (separator (if (org-at-table-hline-p) "[+|]" "|")))
          (while (re-search-forward separator pos t) (cl-incf column))
          column)))))

(defun org-table-goto-column (n &optional on-delim force)
  "Move the cursor to the Nth column in the current table line.
With optional argument ON-DELIM, stop with point before the left delimiter
of the field.
If there are less than N fields, just go to after the last delimiter.
However, when FORCE is non-nil, create new columns if necessary."
  (interactive "p")
  (beginning-of-line 1)
  (when (> n 0)
    (while (and (> (setq n (1- n)) -1)
                (or (search-forward "|" (point-at-eol) t)
                    (and force
                         (progn (end-of-line 1)
                                (skip-chars-backward "^|")
                                (insert " | ")
                                t)))))
    (when (and force (not (looking-at ".*|")))
      (save-excursion (end-of-line 1) (insert " | ")))
    (if on-delim
        (backward-char 1)
      (if (looking-at " ") (forward-char 1)))))
