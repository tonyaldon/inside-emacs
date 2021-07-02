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

;;; ta-avy-goto-end-of-line

(require 'avy)

(defun ta-avy-goto-end-of-line ()
  "Call `avy-goto-char' with \"\n\" as argument."
  (interactive)
  (avy-goto-char ?\n))
