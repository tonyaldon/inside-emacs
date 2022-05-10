;;; get starded with company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

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
