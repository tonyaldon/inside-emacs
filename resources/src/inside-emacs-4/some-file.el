(defun my-readme ()
  "docstring"
  (interactive)
  (delete-other-windows)
	(let ((file-name "./README.org"))
		(find-file file-name)))
