;;; part of org.el
;; find this command in the original file `lisp/org/org.el'
;; Emacs source repository
;; $ git clone https://git.savannah.gnu.org/git/emacs.git
;; GNU Emacs source code and development is hosted on .

(defun org-cycle (&optional arg)
  "TAB-action and visibility cycling for Org mode.

This is the command invoked in Org mode by the `TAB' key.  Its main
purpose is outline visibility cycling, but it also invokes other actions
in special contexts.

When this function is called with a `\\[universal-argument]' prefix, rotate \
the entire
buffer through 3 states (global cycling)
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, \
switch to the startup visibility,
determined by the variable `org-startup-folded', and by any VISIBILITY
properties in the buffer.

With a `\\[universal-argument] \\[universal-argument] \
\\[universal-argument]' prefix argument, show the entire buffer, including
any drawers.

When inside a table, re-align the table and move to the next field.

When point is at the beginning of a headline, rotate the subtree started
by this line through 3 different states (local cycling)
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.
               From this state, you can move to one of the children
               and zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.
If there is no subtree, switch directly from CHILDREN to FOLDED.

When point is at the beginning of an empty headline and the variable
`org-cycle-level-after-item/entry-creation' is set, cycle the level
of the headline by demoting and promoting it to likely levels.  This
speeds up creation document structure by pressing `TAB' once or several
times right after creating a new headline.

When there is a numeric prefix, go up to a heading with level ARG, do
a `show-subtree' and return to the previous cursor position.  If ARG
is negative, go up that many levels.

When point is not at the beginning of a headline, execute the global
binding for `TAB', which is re-indenting the line.  See the option
`org-cycle-emulate-tab' for details.

As a special case, if point is at the beginning of the buffer and there is
no headline in line 1, this function will act as if called with prefix arg
\(`\\[universal-argument] TAB', same as `S-TAB') also when called without \
prefix arg, but only
if the variable `org-cycle-global-at-bob' is t."
  (interactive "P")
  (org-load-modules-maybe)
  (unless (or (run-hook-with-args-until-success 'org-tab-first-hook)
							(and org-cycle-level-after-item/entry-creation
									 (or (org-cycle-level)
											 (org-cycle-item-indentation))))
    (let* ((limit-level
						(or org-cycle-max-level
								(and (boundp 'org-inlinetask-min-level)
										 org-inlinetask-min-level
										 (1- org-inlinetask-min-level))))
					 (nstars (and limit-level
												(if org-odd-levels-only
														(and limit-level (1- (* limit-level 2)))
													limit-level)))
					 (org-outline-regexp
						(if (not (derived-mode-p 'org-mode))
								outline-regexp
							(concat "\\*" (if nstars (format "\\{1,%d\\} " nstars) "+ "))))
					 (bob-special (and org-cycle-global-at-bob (not arg) (bobp)
														 (not (looking-at org-outline-regexp))))
					 (org-cycle-hook
						(if bob-special
								(delq 'org-optimize-window-after-visibility-change
											(copy-sequence org-cycle-hook))
							org-cycle-hook))
					 (pos (point)))

      (cond

       ((equal arg '(16))
				(setq last-command 'dummy)
				(org-set-startup-visibility)
				(org-unlogged-message "Startup visibility, plus VISIBILITY properties"))

       ((equal arg '(64))
				(org-show-all)
				(org-unlogged-message "Entire buffer visible, including drawers"))

       ((equal arg '(4)) (org-cycle-internal-global))

       ;; Try hiding block at point.
       ((org-hide-block-toggle-maybe))

       ;; Try cdlatex TAB completion
       ((org-try-cdlatex-tab))

       ;; Table: enter it or move to the next field.
       ((org-at-table-p 'any)
				(if (org-at-table.el-p)
						(message "%s" (substitute-command-keys "\\<org-mode-map>\
Use `\\[org-edit-special]' to edit table.el tables"))
					(if arg (org-table-edit-field t)
						(org-table-justify-field-maybe)
						(call-interactively 'org-table-next-field))))

       ((run-hook-with-args-until-success 'org-tab-after-check-for-table-hook))

       ;; Global cycling: delegate to `org-cycle-internal-global'.
       (bob-special (org-cycle-internal-global))

       ;; Drawers: delegate to `org-flag-drawer'.
       ((save-excursion
					(beginning-of-line 1)
					(looking-at org-drawer-regexp))
				(org-flag-drawer		; toggle block visibility
				 (not (get-char-property (match-end 0) 'invisible))))

       ;; Show-subtree, ARG levels up from here.
       ((integerp arg)
				(save-excursion
					(org-back-to-heading)
					(outline-up-heading (if (< arg 0) (- arg)
																(- (funcall outline-level) arg)))
					(org-show-subtree)))

       ;; Inline task: delegate to `org-inlinetask-toggle-visibility'.
       ((and (featurep 'org-inlinetask)
						 (org-inlinetask-at-task-p)
						 (or (bolp) (not (eq org-cycle-emulate-tab 'exc-hl-bol))))
				(org-inlinetask-toggle-visibility))

       ;; At an item/headline: delegate to `org-cycle-internal-local'.
       ((and (or (and org-cycle-include-plain-lists (org-at-item-p))
								 (save-excursion (move-beginning-of-line 1)
																 (looking-at org-outline-regexp)))
						 (or (bolp) (not (eq org-cycle-emulate-tab 'exc-hl-bol))))
				(org-cycle-internal-local))

       ;; From there: TAB emulation and template completion.
       (buffer-read-only (org-back-to-heading))

       ((run-hook-with-args-until-success
				 'org-tab-after-check-for-cycling-hook))

       ((run-hook-with-args-until-success
				 'org-tab-before-tab-emulation-hook))

       ((and (eq org-cycle-emulate-tab 'exc-hl-bol)
						 (or (not (bolp))
								 (not (looking-at org-outline-regexp))))
				(call-interactively (global-key-binding "\t")))

       ((if (and (memq org-cycle-emulate-tab '(white whitestart))
								 (save-excursion (beginning-of-line 1) (looking-at "[ \t]*"))
								 (or (and (eq org-cycle-emulate-tab 'white)
													(= (match-end 0) (point-at-eol)))
										 (and (eq org-cycle-emulate-tab 'whitestart)
													(>= (match-end 0) pos))))
						t
					(eq org-cycle-emulate-tab t))
				(call-interactively (global-key-binding "\t")))

       (t (save-excursion
						(org-back-to-heading)
						(org-cycle)))))))

(defun org-return (&optional indent)
  "Goto next table row or insert a newline.

Calls `org-table-next-row' or `newline', depending on context.

When optional INDENT argument is non-nil, call
`newline-and-indent' instead of `newline'.

When `org-return-follows-link' is non-nil and point is on
a timestamp or a link, call `org-open-at-point'.  However, it
will not happen if point is in a table or on a \"dead\"
object (e.g., within a comment).  In these case, you need to use
`org-open-at-point' directly."
  (interactive)
  (let ((context (if org-return-follows-link (org-element-context)
									 (org-element-at-point))))
    (cond
     ;; In a table, call `org-table-next-row'.  However, before first
     ;; column or after last one, split the table.
     ((or (and (eq 'table (org-element-type context))
							 (not (eq 'table.el (org-element-property :type context)))
							 (>= (point) (org-element-property :contents-begin context))
							 (< (point) (org-element-property :contents-end context)))
					(org-element-lineage context '(table-row table-cell) t))
      (if (or (looking-at-p "[ \t]*$")
							(save-excursion (skip-chars-backward " \t") (bolp)))
					(insert "\n")
				(org-table-justify-field-maybe)
				(call-interactively #'org-table-next-row)))
     ;; On a link or a timestamp, call `org-open-at-point' if
     ;; `org-return-follows-link' allows it.  Tolerate fuzzy
     ;; locations, e.g., in a comment, as `org-open-at-point'.
     ((and org-return-follows-link
					 (or (and (eq 'link (org-element-type context))
										;; Ensure point is not on the white spaces after
										;; the link.
										(let ((origin (point)))
											(org-with-point-at (org-element-property :end context)
												(skip-chars-backward " \t")
												(> (point) origin))))
							 (org-in-regexp org-ts-regexp-both nil t)
							 (org-in-regexp org-tsr-regexp-both nil  t)
							 (org-in-regexp org-link-any-re nil t)))
      (call-interactively #'org-open-at-point))
     ;; Insert newline in heading, but preserve tags.
     ((and (not (bolp))
					 (let ((case-fold-search nil))
						 (org-match-line org-complex-heading-regexp)))
      ;; At headline.  Split line.  However, if point is on keyword,
      ;; priority cookie or tags, do not break any of them: add
      ;; a newline after the headline instead.
      (let ((tags-column (and (match-beginning 5)
															(save-excursion (goto-char (match-beginning 5))
																							(current-column))))
						(string
						 (when (and (match-end 4) (org-point-in-group (point) 4))
							 (delete-and-extract-region (point) (match-end 4)))))
				;; Adjust tag alignment.
				(cond
				 ((not (and tags-column string)))
				 (org-auto-align-tags (org-align-tags))
				 (t (org--align-tags-here tags-column))) ;preserve tags column
				(end-of-line)
				(org-show-entry)
				(if indent (newline-and-indent) (newline))
				(when string (save-excursion (insert (org-trim string))))))
     ;; In a list, make sure indenting keeps trailing text within.
     ((and indent
					 (not (eolp))
					 (org-element-lineage context '(item)))
      (let ((trailing-data
						 (delete-and-extract-region (point) (line-end-position))))
				(newline-and-indent)
				(save-excursion (insert trailing-data))))
     (t
      ;; Do not auto-fill when point is in an Org property drawer.
      (let ((auto-fill-function (and (not (org-at-property-p))
																		 auto-fill-function)))
				(if indent
						(newline-and-indent)
					(newline)))))))


(defun org-backward-sentence (&optional _arg)
  "Go to beginning of sentence, or beginning of table field.
This will call `backward-sentence' or `org-table-beginning-of-field',
depending on context."
  (interactive)
  (let* ((element (org-element-at-point))
				 (contents-begin (org-element-property :contents-begin element))
				 (table (org-element-lineage element '(table) t)))
    (if (and table
						 (> (point) contents-begin)
						 (<= (point) (org-element-property :contents-end table)))
				(call-interactively #'org-table-beginning-of-field)
      (save-restriction
				(when (and contents-begin
									 (< (point-min) contents-begin)
									 (> (point) contents-begin))
					(narrow-to-region contents-begin
														(org-element-property :contents-end element)))
				(call-interactively #'backward-sentence)))))

(defun org-forward-sentence (&optional _arg)
  "Go to end of sentence, or end of table field.
This will call `forward-sentence' or `org-table-end-of-field',
depending on context."
  (interactive)
  (if (and (org-at-heading-p)
					 (save-restriction (skip-chars-forward " \t") (not (eolp))))
      (save-restriction
				(narrow-to-region (line-beginning-position) (line-end-position))
				(call-interactively #'forward-sentence))
    (let* ((element (org-element-at-point))
					 (contents-end (org-element-property :contents-end element))
					 (table (org-element-lineage element '(table) t)))
      (if (and table
							 (>= (point) (org-element-property :contents-begin table))
							 (< (point) contents-end))
					(call-interactively #'org-table-end-of-field)
				(save-restriction
					(when (and contents-end
										 (> (point-max) contents-end)
										 ;; Skip blank lines between elements.
										 (< (org-element-property :end element)
												(save-excursion (goto-char contents-end)
																				(skip-chars-forward " \r\t\n"))))
						(narrow-to-region (org-element-property :contents-begin element)
															contents-end))
					;; End of heading is considered as the end of a sentence.
					(let ((sentence-end (concat (sentence-end) "\\|^\\*+ .*$")))
						(call-interactively #'forward-sentence)))))))

(define-derived-mode org-mode outline-mode "Org"
  "Outline-based notes management and organizer, alias
\"Carsten's outline-mode for keeping track of everything.\"

Org mode develops organizational tasks around a NOTES file which
contains information about projects as plain text.  Org mode is
implemented on top of Outline mode, which is ideal to keep the content
of large files well structured.  It supports ToDo items, deadlines and
time stamps, which magically appear in the diary listing of the Emacs
calendar.  Tables are easily created with a built-in table editor.
Plain text URL-like links connect to websites, emails (VM), Usenet
messages (Gnus), BBDB entries, and any files related to the project.
For printing and sharing of notes, an Org file (or a part of it)
can be exported as a structured ASCII or HTML file.

The following commands are available:

\\{org-mode-map}"
  (org-load-modules-maybe)
  (org-install-agenda-files-menu)
  (when org-link-descriptive (add-to-invisibility-spec '(org-link)))
  (add-to-invisibility-spec '(org-hide-block . t))
  (add-to-invisibility-spec '(org-hide-drawer . t))
  (setq-local outline-regexp org-outline-regexp)
  (setq-local outline-level 'org-outline-level)
  (setq bidi-paragraph-direction 'left-to-right)
  (when (and (stringp org-ellipsis) (not (equal "" org-ellipsis)))
    (unless org-display-table
      (setq org-display-table (make-display-table)))
    (set-display-table-slot
     org-display-table 4
     (vconcat (mapcar (lambda (c) (make-glyph-code c 'org-ellipsis))
											org-ellipsis)))
    (setq buffer-display-table org-display-table))
  (org-set-regexps-and-options)
  (org-set-font-lock-defaults)
  (when (and org-tag-faces (not org-tags-special-faces-re))
    ;; tag faces set outside customize.... force initialization.
    (org-set-tag-faces 'org-tag-faces org-tag-faces))
  ;; Calc embedded
  (setq-local calc-embedded-open-mode "# ")
  ;; Modify a few syntax entries
  (modify-syntax-entry ?\" "\"")
  (modify-syntax-entry ?\\ "_")
  (modify-syntax-entry ?~ "_")
  (modify-syntax-entry ?< "(>")
  (modify-syntax-entry ?> ")<")
  (setq-local font-lock-unfontify-region-function 'org-unfontify-region)
  ;; Activate before-change-function
  (setq-local org-table-may-need-update t)
  (add-hook 'before-change-functions 'org-before-change-function nil 'local)
  ;; Check for running clock before killing a buffer
  (add-hook 'kill-buffer-hook 'org-check-running-clock nil 'local)
  ;; Initialize macros templates.
  (org-macro-initialize-templates)
  ;; Initialize radio targets.
  (org-update-radio-target-regexp)
  ;; Indentation.
  (setq-local indent-line-function 'org-indent-line)
  (setq-local indent-region-function 'org-indent-region)
  ;; Filling and auto-filling.
  (org-setup-filling)
  ;; Comments.
  (org-setup-comments-handling)
  ;; Initialize cache.
  (org-element-cache-reset)
  ;; Beginning/end of defun
  (setq-local beginning-of-defun-function 'org-backward-element)
  (setq-local end-of-defun-function
							(lambda ()
								(if (not (org-at-heading-p))
										(org-forward-element)
									(org-forward-element)
									(forward-char -1))))
  ;; Next error for sparse trees
  (setq-local next-error-function 'org-occur-next-match)
  ;; Make commit log messages from Org documents easier.
  (setq-local add-log-current-defun-function #'org-add-log-current-headline)
  ;; Make sure dependence stuff works reliably, even for users who set it
  ;; too late :-(
  (if org-enforce-todo-dependencies
      (add-hook 'org-blocker-hook
								'org-block-todo-from-children-or-siblings-or-parent)
    (remove-hook 'org-blocker-hook
								 'org-block-todo-from-children-or-siblings-or-parent))
  (if org-enforce-todo-checkbox-dependencies
      (add-hook 'org-blocker-hook
								'org-block-todo-from-checkboxes)
    (remove-hook 'org-blocker-hook
								 'org-block-todo-from-checkboxes))

  ;; Align options lines
  (setq-local
   align-mode-rules-list
   '((org-in-buffer-settings
      (regexp . "^[ \t]*#\\+[A-Z_]+:\\(\\s-*\\)\\S-+")
      (modes . '(org-mode)))))

  ;; Make isearch reveal context
  (setq-local outline-isearch-open-invisible-function
							(lambda (&rest _) (org-show-context 'isearch)))

  ;; Setup the pcomplete hooks
  (setq-local pcomplete-command-completion-function #'org-pcomplete-initial)
  (setq-local pcomplete-command-name-function #'org-command-at-point)
  (setq-local pcomplete-default-completion-function #'ignore)
  (setq-local pcomplete-parse-arguments-function #'org-parse-arguments)
  (setq-local pcomplete-termination-string "")
  (add-hook 'completion-at-point-functions
            #'pcomplete-completions-at-point nil t)
  (setq-local buffer-face-mode-face 'org-default)

  ;; If empty file that did not turn on Org mode automatically, make
  ;; it to.
  (when (and org-insert-mode-line-in-empty-file
						 (called-interactively-p 'any)
						 (= (point-min) (point-max)))
    (insert "#    -*- mode: org -*-\n\n"))
  (unless org-inhibit-startup
    (org-unmodified
     (when org-startup-with-beamer-mode (org-beamer-mode))
     (when (or org-startup-align-all-tables org-startup-shrink-all-tables)
       (org-table-map-tables
				(cond ((and org-startup-align-all-tables
										org-startup-shrink-all-tables)
							 (lambda () (org-table-align) (org-table-shrink)))
							(org-startup-align-all-tables #'org-table-align)
							(t #'org-table-shrink))
				t))
     (when org-startup-with-inline-images (org-display-inline-images))
     (when org-startup-with-latex-preview (org-latex-preview '(16)))
     (unless org-inhibit-startup-visibility-stuff (org-set-startup-visibility))
     (when org-startup-truncated (setq truncate-lines t))
     (when org-startup-indented (require 'org-indent) (org-indent-mode 1))))
  ;; Try to set `org-hide' face correctly.
  (let ((foreground (org-find-invisible-foreground)))
    (when foreground
      (set-face-foreground 'org-hide foreground))))
