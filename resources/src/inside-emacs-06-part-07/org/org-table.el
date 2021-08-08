;;; part of org
;; You can find this function in the original file `lisp/org/org-table.el'
;; Emacs source repository
;; $ git clone https://git.savannah.gnu.org/git/emacs.git

(defun orgtbl-to-html (table params)
  "Convert the orgtbl-mode TABLE to HTML.

TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the
conversion.  All parameters from `orgtbl-to-generic' are
supported.  It is also possible to use the following one:

:attributes

  Attributes and values, as a plist, which will be used in
  <table> tag."
  (require 'ox-html)
  (orgtbl-to-generic
   table
   (org-combine-plists
    ;; Provide sane default values.
    (list :backend 'html
          :html-table-data-tags '("<td%s>" . "</td>")
          :html-table-use-header-tags-for-first-column nil
          :html-table-align-individual-fields t
          :html-table-row-tags '("<tr>" . "</tr>")
          :html-table-attributes
          (if (plist-member params :attributes)
              (plist-get params :attributes)
            '(:border "2" :cellspacing "0" :cellpadding "6" :rules "groups"
              :frame "hsides")))
    params)))
