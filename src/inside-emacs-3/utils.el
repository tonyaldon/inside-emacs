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

(defun ta-aw-other-window-scroll-buffer ()
  "Use `ace-window' to set `other-window-scroll-buffer'."
  (interactive)
  (let ((initial-window (selected-window)))
    (save-excursion
      (call-interactively 'ace-window)
      (setq other-window-scroll-buffer (current-buffer)))
    (select-window initial-window)))

(defun ta-aw-reset-other-window-scroll-buffer ()
  "Reset `other-window-scroll-buffer' to nil when not a displayed buffer.

Use this function to advice `scroll-other-window' and `scroll-other-window-down'
before. This prevent to popup the buffer `other-window-scroll-buffer' if it
was not being displayed."
  (when (and other-window-scroll-buffer
						 (not (get-buffer-window other-window-scroll-buffer)))
		(setq other-window-scroll-buffer nil)))

(defadvice scroll-other-window
		(before ta-aw-reset-other-window-scroll-buffer-advice activate)
	"Reset `other-window-scroll-buffer' to nil when not a displayed buffer.

This prevent to popup the buffer `other-window-scroll-buffer' if it
was not being displayed.

See `ta-aw-other-window-scroll-buffer'."
  (ta-aw-reset-other-window-scroll-buffer))

(defadvice scroll-other-window-down
		(before ta-aw-reset-other-window-scroll-buffer-advice activate)
	"Reset `other-window-scroll-buffer' to nil when not a displayed buffer.

This prevent to popup the buffer `other-window-scroll-buffer' if it
was not being displayed.

See `ta-aw-other-window-scroll-buffer'."
  (ta-aw-reset-other-window-scroll-buffer))
