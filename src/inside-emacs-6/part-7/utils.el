;;; handy-expand-region-dwim

(require 'expand-region)

(defun handy-expand-region-dwim (arg)
  "If region is active, call `er/expand-region'.  If not call `er/mark-word'."
  (interactive "p")
  (if (or (region-active-p) (equal last-command this-command))
      (er/expand-region arg)
    (er/mark-word)
    (if (equal arg 4) (exchange-point-and-mark))))

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

;;; handy-mark-inside-dwim

(require 'smartparens)
(require 'expand-region)

(defun handy-point-in-url-p ()
  "Return the url at `point' if `point' is in an url."
  (thing-at-point 'url))

(defun handy-mark-goto-beginning-of-string (pt)
  "Go to begining of the string if PT is inside a string.

Return nil if PT isn't inside a string.
See the function `handy-point-in-string-p'"
  (if (handy-point-in-string-p pt)
      (goto-char (nth 8 (syntax-ppss pt)))
    nil))

(defun handy-mark-inside-pairs ()
  "An other way to do `er/mark-inside-pairs' but work for sgml-tag too."
  (interactive)
  (handy-mark-goto-beginning-of-string (point))
  ;; todo: do thing when inside a tag <tag name="tony"> (maybe use the function sgml-begining-of-tag)
  (sp-backward-up-sexp)
  (sp-mark-sexp)
  (sp-down-sexp)
  (exchange-point-and-mark)
  (sp-backward-down-sexp)
  (exchange-point-and-mark))

(defun handy-mark-inside-field ()
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

(defun handy-mark-inside-dwim (&optional arg)
  "Mark things inside quotes if point is inside a string.

If not inside string, mark inside table field in `org-mode'.
In other modes, mark things inside pairs.
If call two times consecutively mark inside pairs."
  (interactive)
  (cond ((equal last-command this-command)
         (call-interactively 'handy-mark-inside-pairs))
        ((er--point-inside-string-p)
         (call-interactively 'er/mark-inside-quotes))
        ((and (equal major-mode 'org-mode) (org-at-table-p))
         (handy-mark-inside-field))
        (t (call-interactively 'handy-mark-inside-pairs))))

;;; insight-scroll-up-half-window

(defun insight--half-window-height ()
  "Compute half window height."
  (/ (window-body-height) 2))

(defun insight-scroll-up-half-window ()
  "Scroll up of half window height."
  (interactive)
  (scroll-up (insight--half-window-height)))
;;; ta-avy-goto-end-of-line

(require 'avy)

(defun ta-avy-goto-end-of-line ()
  "Call `avy-goto-char' with \"\n\" as argument."
  (interactive)
  (avy-goto-char ?\n))

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
