(defun ta-mark-inside-pairs ()
  "An other way to do `er/mark-inside-pairs' but work for sgml-tag too."
  (interactive)
  (ta-goto-begining-of-string (point))
  (sp-backward-up-sexp)
  (sp-mark-sexp)
  (sp-down-sexp)
  (exchange-point-and-mark)
  (sp-backward-down-sexp)
  (exchange-point-and-mark))

(defun ta-mark-inside-quotes-or-pairs (&optional arg)
  "Mark inside quotes calling `er/mark-inside-quotes'.

When ARG is 4, mark inside pairs calling `ta-mark-inside-pairs'."
  (interactive "p")
  (if (equal arg 4)
			(call-interactively 'ta-mark-inside-pairs)
		(call-interactively 'er/mark-inside-quotes)))

(defun ta-toggle-write-mode ()
  "Toggle to the Writable variant of the current mode.

Call command `dired-toggle-read-only' if `major-mode' is equal
`dired-mode' and call command `wgrep-change-to-wgrep-mode' if
`major-mode' is equal to `grep-mode'."
  (interactive)
  (cond ((string-equal major-mode "dired-mode")
         (call-interactively 'dired-toggle-read-only))
        ((memq major-mode '(grep-mode ivy-occur-grep-mode))
         (call-interactively 'wgrep-change-to-wgrep-mode))
        (t (message "You have to be in either in `dired-mode' or
`grep-mode' to execute this command"))))

(defun ta-w-finish-edit ()
  "Abort changes and return to the appropiate mode.

Call command `wdired-finish-edit' if `major-mode' is
`wdired-mode' and call command `wgrep-finish-edit' if
`major-mode' is `grep-mode'."
  (interactive)
  (cond ((string-equal major-mode "wdired-mode")
         (call-interactively 'wdired-finish-edit))
        ((memq major-mode '(grep-mode ivy-occur-grep-mode))
         (call-interactively 'wgrep-finish-edit))
        (t (message "You have to be in either in `wdired-mode' or
`grep-mode' to execute this command"))))
