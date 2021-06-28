(defun indent-buffer (&rest _r)
  "Indent the whole buffer."
  (let ((inhibit-message t))
    (indent-region (point-min) (point-max))))

(advice-add 'orgtbl-insert-radio-table :after 'indent-buffer)
(advice-add 'orgtbl-ctrl-c-ctrl-c :after 'indent-buffer)
