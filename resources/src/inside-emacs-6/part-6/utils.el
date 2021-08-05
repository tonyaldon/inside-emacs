;;; handy-mark-line

(defun handy-line-copy ()
  "Copy current line."
  (interactive)
  (copy-region-as-kill (point-at-bol) (point-at-eol)))

(defun handy-mark-line (arg)
  "Mark the current line.

If call with `universal-argument', copy the line."
  (interactive "p")
  (if (equal arg 4)
      (handy-line-copy)
    ;; HACK: Have to use both `push-mark' and `set-mark' in this order
    ;;       to get expected result.
    (end-of-line)
    (push-mark (point))
    (set-mark (point))
    (beginning-of-line)))

;;; handy-mark-dwim

(require 'smartparens)
(require 'expand-region)

(defun handy-point-in-url-p ()
  "Return the url at `point' if `point' is in an url."
  (thing-at-point 'url))

(defun handy-point-at-beginning-of-sexp-delimited-by-pairs-p ()
  "Return t if point is at beginning of sexp delimited by pairs."
  (let ((sexp (sp-get-thing)))
    (and (eq (point) (sp-get sexp :beg))
         (not (string-empty-p (sp-get sexp :op))))))

(defun handy-mark-sexp-at-point ()
  "Mark the `sexp' at point."
  (let ((sexp-beg (beginning-of-thing 'sexp))
        (sexp-end (end-of-thing 'sexp)))
    (goto-char sexp-end)
    ;; HACK: Have to use both `push-mark' and `set-mark' in this order to
    ;;       expected result.
    (push-mark sexp-end)
    (set-mark sexp-end)
    (goto-char sexp-beg)))

(defun handy-mark-dwim (arg)
  "Mark the url, sexp or sentence at point.

If point is in a url, call `er/mark-url'.  If not mark sexp at point.
If call 2 times consecutively, call `er/mark-sentence'."
  (interactive "p")
  (cond
   ((equal last-command this-command)
    (er/mark-sentence))
   ((handy-point-in-url-p)
    (er/mark-url))
   ((or (handy-point-at-beginning-of-sexp-delimited-by-pairs-p)
        (eq (following-char) ?<))
    (sp-mark-sexp))
   ((eq (preceding-char) ?\")
    (sp-backward-sexp)
    (sp-mark-sexp))
   ((and (memq (following-char) '(32 ?\) ?\] ?\} ?>))
         (looking-back "[[:alnum:]]" 1))
    (backward-char 1)
    (handy-mark-sexp-at-point))
   (t (handy-mark-sexp-at-point)))
  (if (equal arg 4) (exchange-point-and-mark)))

;;; handy-expand-region-dwim

(require 'expand-region)

(defun handy-expand-region-dwim (arg)
  "If region is active, call `er/expand-region'.  If not call `er/mark-word'."
  (interactive "p")
  (if (or (region-active-p) (equal last-command this-command))
      (er/expand-region arg)
    (er/mark-word)
    (if (equal arg 4) (exchange-point-and-mark))))

;;; ta-describe-thing-at-point

(defun ta-describe-thing-at-point ()
  "Display the full documentation of the `thing-at-point'.

Return nil if the symbol of the `thing-at-point' is neither a function
nor a variable."
  (interactive)
  (when-let* ((symbol (symbol-at-point))
              (symbol-n (symbol-name symbol)))
    (when (and (eq major-mode 'org-mode)
               (s-starts-with-p "~" symbol-n)
               (s-ends-with-p "~" symbol-n))
      (setq symbol (->> symbol-n
                        (s-chop-prefix "~")
                        (s-chop-suffix "~")
                        (intern))))
    (describe-symbol symbol)))

;;; ta-org-shiftmetadown

(require 'org)

(defun ta-org-shiftmetadown (&optional _arg)
  "Drag the line at point down.
In a table, insert an empty row below the current line (this part
differs from the original `org-shiftmetadown' command).
On a clock timestamp, update the value of the timestamp like `S-<down>'
but also adjust the previous clocked item in the clock history.
Everywhere else, drag the line at point down."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetadown-hook))
   ((org-at-table-p) (org-table-insert-row 'below))
   ((org-at-clock-log-p) (let ((org-clock-adjust-closest t))
                           (call-interactively 'org-timestamp-down)))
   (t (call-interactively 'org-drag-line-forward))))

;;; insight-scroll-up-half-window

(defun insight--half-window-height ()
  "Compute half window height."
  (/ (window-body-height) 2))

(defun insight-scroll-up-half-window ()
  "Scroll up of half window height."
  (interactive)
  (scroll-up (insight--half-window-height)))
