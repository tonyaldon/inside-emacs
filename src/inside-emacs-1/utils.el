(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save.
see: https://github.com/magnars/.emacs.d/blob/master/defuns/buffer-defuns.el#L144-166"
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

(defun ta-avy-goto-end-of-line ()
  "Call `avy-goto-char' with \"\n\" as argument."
  (interactive)
  (avy-goto-char ?\n))

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

(defun ta-toggle-narrow ()
  "Toggle between `widen' and `org-narrow-to-subtree'."
  (interactive)
  (if (buffer-narrowed-p) (widen)
    (org-narrow-to-subtree)))

(defun ta-yank-line-below ()
  "copy current line and yank it to the next line.
Cursor doesn't move."
  (interactive)
  (setq init-point (point))
  (save-excursion
    (beginning-of-line)
    (setq beg-point (point))
    (end-of-line)
    (setq end-point (point))
    (setq line-text (delete-and-extract-region end-point beg-point))
    (insert line-text)
    (newline)
    (insert line-text))
  (goto-char init-point))
