;;; part of org
;; You can find this function in the original file `lisp/org/org-table.el'
;; Emacs source repository
;; $ git clone https://git.savannah.gnu.org/git/emacs.git

(defun orgtbl-to-tsv (table params)
  "Convert the orgtbl-mode table to TAB separated material."
  (orgtbl-to-generic table (org-combine-plists '(:sep "\t") params)))
