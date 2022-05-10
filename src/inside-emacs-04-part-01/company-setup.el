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