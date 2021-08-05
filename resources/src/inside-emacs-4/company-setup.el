;;; get starded with company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;; set custom faces
(let ((black-1 "#151515")
      (black-2 "#333333")
      (gray    "#8c8c8c")
      (white-1 "#dedede")
      (white-0 "ffffff")
      (orange  "#fd971f")
      (cyan-1  "#017a7e")
      (cyan-2  "#02eaf3"))
  (custom-set-faces
   `(company-preview ((t (:foreground ,white-1 :background ,black-2 :bold t))))
   `(company-preview-search ((t (:foreground ,orange :background ,black-2 :bold t))))
   `(company-preview-common ((t (:foreground ,white-0 :background ,black-2 :bold t))))
   `(company-scrollbar-bg ((t (:background ,black-2))))
   `(company-scrollbar-fg ((t (:foreground ,white-0 :bold t))))
   `(company-tooltip ((t (:foreground ,gray :background ,black-1 ))))
   `(company-tooltip-common ((t (:foreground ,cyan-1 :weight bold))))
   `(company-tooltip-common-selection ((t (:foreground ,cyan-2 :weight bold))))
   `(company-tooltip-selection ((t (:foreground ,white-1 :bold t))))
   `(company-tooltip-annotation ((t (:foreground ,orange))))
   `(company-tooltip-annotation-selection ((t (:foreground ,orange))))
   `(company-tooltip-search ((t (:foreground ,orange :bold t))))
   `(company-tooltip-search-selection ((t (:foreground ,orange :bold t))))))

;;;company-backends variable
(defun company-emacs-lisp-mode ()
  "Set up `company-mode' for `emacs-lisp-mode'."
  (set (make-local-variable 'company-backends)
			 '((company-yasnippet
					:with
          company-elisp
          company-dabbrev-code
          company-files))))

(add-hook 'emacs-lisp-mode-hook 'company-emacs-lisp-mode)

(defun company-text-mode ()
  "Set up `company-mode' for `text-mode'."
	(setq company-minimum-prefix-length 3)
	(set (make-local-variable 'company-backends)
       '((company-files) company-dabbrev)))

(add-hook 'text-mode-hook 'company-text-mode)

;;; customize company frontend variables
(setq company-idle-delay 0)
(setq company-selection-wrap-around t)
(setq company-tooltip-limit 6)

(setq company-minimum-prefix-length 1)
(make-variable-buffer-local 'company-minimum-prefix-length)

(setq company-transformers '(company-sort-by-backend-importance))

;;; company-active-map
(define-key company-active-map (kbd ">") 'company-filter-candidates)
