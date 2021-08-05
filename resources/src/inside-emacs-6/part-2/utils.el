;;; ta-avy-copy-sexp
(require 'avy)
(require 'smartparens)

(defun ta-avy-copy-sexp ()
  "Copy a selected sexp at the current point"
  (interactive)
  (let ((initial-window (selected-window)))
    (save-excursion
      (call-interactively 'avy-goto-word-or-subword-1)
      (sp-copy-sexp))
    (select-window initial-window)
    (yank)))

;;; ta-avy-goto-end-of-line
(require 'avy)

(defun ta-avy-goto-end-of-line ()
  "Call `avy-goto-char' with \"\n\" as argument."
  (interactive)
  (avy-goto-char ?\n))

;;; ta-mark-inside-dwim
(require 'expand-region)
(require 'smartparens)

(defun ta-mark-inside-org-table ()
  "Mark current field inside org-table."
  (interactive)
  (when (org-at-table-p)
    (push-mark (point))
		(re-search-forward "|")
		(backward-char)
		(skip-chars-backward " ")
		(push-mark (point))
		(set-mark (point))
		(org-table-beginning-of-field 1)))

(defun ta-mark-inside-pairs ()
  "An other way to do `er/mark-inside-pairs' but work for sgml-tag too."
  (interactive)
  (ta-goto-begining-of-string (point))
  ;; todo: do thing when inside a tag <tag name="tony"> (maybe use the function sgml-begining-of-tag)
  (sp-backward-up-sexp)
  (sp-mark-sexp)
  (sp-down-sexp)
  (exchange-point-and-mark)
  (sp-backward-down-sexp)
  (exchange-point-and-mark))


(defun ta-mark-inside-dwim (&optional arg)
  "Mark things inside quotes if point is inside a string.

If not inside string, mark inside table field in `org-mode'.
In other modes, mark things inside pairs.
If call two times consecutively mark inside pairs."
  (interactive)
  (cond ((equal last-command 'ta-mark-inside-dwim)
				 (call-interactively 'ta-mark-inside-pairs))
				((er--point-inside-string-p) ;FIXME: return nil inside string in markdown-mode
				 (call-interactively 'er/mark-inside-quotes))
				((and (equal major-mode 'org-mode) (org-at-table-p))
				 (ta-mark-inside-org-table))
				(t (call-interactively 'ta-mark-inside-pairs))))

;;; ta-mark-sexp-at-point
(require 'smartparens)

(defun ta--point-at-beginnig-sp-sexp-p ()
  "Return non-nil if `point' is at the beginning of a sp-sexp

and :op non empty. See `sp-get-thing'."
  (let ((ok (sp-get-thing)))
    (when ok
      (when (and (eq (point) (sp-get ok :beg))
                 (not (string-empty-p (sp-get ok :op))))
        (point)))))

(defun ta--mark-sexp-at-point ()
  "Mark the `sexp' at point."
  (let ((sexp-beg (beginning-of-thing 'sexp))
        (sexp-end (end-of-thing 'sexp)))
    (goto-char sexp-end)
    (set-mark sexp-end)
    (goto-char sexp-beg)))

(defun ta-mark-sexp-at-point ()
  "Mark the `sexp' at point. See `sexp-at-point' and `sp-mark-sexp'
(smartparens packages)."
  (interactive)
  (if (or (ta--point-at-beginnig-sp-sexp-p)
          (eq (following-char) ?<))
      (sp-mark-sexp)
    (if (eq (preceding-char) ?\")
        (progn
          (sp-backward-sexp)
          (sp-mark-sexp))
      (if (and (memq (following-char) '(32 ?\) ?\] ?\} ?>))
               (looking-back "[[:alnum:]]" 1))
          (backward-char 1))
      (ta--mark-sexp-at-point))))

;;; other commands
(defun ta-kill-whole-line ()
  "Kill the whole current line.

Preserve the column position of the cursor."
  (interactive)
  (let ((column-position (current-column)))
		(kill-whole-line)
		(move-to-column column-position)))

(defun ta-below-new-indent ()
  "Do `end-of-visual-line' then `newline-and-indent'"
  (interactive)
	(end-of-line)
  (newline-and-indent))
