;;; ta-sidebar
(require 'dired)
(require 'dired-hacks-utils)
(require 'dash)

(defun ta-dired-width (dir)
  "Return the number of characters of the bigger file or directory in

a dired buffer generate with DIR as `dired-directory'."
  (with-current-buffer (dired-noselect dir)
    (-max (--map (length (-last-item (s-split "/" it)))
                 (dired-utils-get-all-files)))))

(defun ta-sidebar ()
  "Pop a buffer on the left of the frame in `dired-mode'

with the parent directory of the current `buffer-file-name' if non-nil
or, your home directory \"~/\".

If the frame contains buffers in `dired-mode', delete them."
  (interactive)
  (let ((initial-window (selected-window))
				dired-window-deleted-p
				buff-file-name)
		(--each (window-list)
			(select-window it)
			(if (string-equal major-mode "dired-mode")
					(progn (delete-window) (setq dired-window-deleted-p t))))
		(unless dired-window-deleted-p
			(select-window initial-window)
			(delete-other-windows)
			(setq buff-file-name
						(file-name-directory (cond (buffer-file-name) ("~/"))))
			(let ((width (ta-dired-width buff-file-name)))
				(split-window-right (+ 10 width))) ; 10 is arbitrary
			(dired buff-file-name))))

;;; ta-outline-toggle-global
(require 'bicycle)

(defun ta-outline-toggle-global ()
  "Toggle visibility of all outline (see `outline-mode') sections.

This command toggle between this following levels:
1. TREES:    Show all headings, treaing top-level code blocks
             as sections (i.e. their first line is treated as
             a heading).
2. ALL:      Show everything, except code blocks that have been
             collapsed individually (using a `hideshow' command
             or function).

This is a variant off the `bicycle-cycle-global' with two
level less."
  (interactive)
  (setq deactivate-mark t)
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward outline-regexp nil t)
      (user-error "Found no heading"))
    (cond
     ((eq last-command 'outline-cycle-trees)
      (outline-show-all)
      (bicycle--message "ALL"))
     (t
      (outline-hide-sublevels (bicycle--level))
      (outline-map-region
       (lambda ()
         (when (bicycle--top-level-p)
           (outline-show-branches)))
       (point-min)
       (point-max))
      (bicycle--message "TREES")
      (setq this-command 'outline-cycle-trees)))))

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

;;; ta-avy-goto-end-of-line
(require 'avy)

(defun ta-avy-goto-end-of-line ()
  "Call `avy-goto-char' with \"\n\" as argument."
  (interactive)
  (avy-goto-char ?\n))

;;; other commands
(defun ta-kill-whole-line ()
  "Kill the whole current line.

Preserve the column position of the cursor."
  (interactive)
  (let ((column-position (current-column)))
		(kill-whole-line)
		(move-to-column column-position)))

(defun ta-above-new-indent ()
  "In the current line, back to indent then split line as `split-line'"
  (interactive)
  (back-to-indentation)
  (split-line))

(defun ta-below-new-indent ()
  "Do `end-of-visual-line' then `newline-and-indent'"
  (interactive)
	(end-of-line)
  (newline-and-indent))
