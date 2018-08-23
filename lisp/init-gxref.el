(when (require 'gxref nil t)
  (add-to-list 'xref-backend-functions 'gxref-xref-backend)

  ;; Each file whose suffix is `.h` is treated as a C++ source file.
  (setenv "GTAGSFORCECPP" "1")

  ;; Redefine without the call to shell-quote-argument to workaround in Windows.
  (defun gxref--find-symbol (symbol &rest args)
    "Run GNU Global to find a symbol SYMBOL.
Return the results as a list of xref location objects.  ARGS are
any additional command line arguments to pass to GNU Global."
    (let* ((process-args
            (append args
                    (list "-x" "-a" symbol)))
           (global-output (gxref--global-to-list process-args)))
      (mapcar #'gxref--make-xref-from-gtags-x-line global-output)
      ))

  ;; Redefine to include "-s" parameter
  (cl-defmethod xref-backend-references ((_backend (eql gxref)) symbol)
    (gxref--find-symbol symbol "-r" "-s"))

  (defun gxref-create-project-db ()
    "Create a GTAGS database in the first directory specified as `project-roots'"
    (interactive)
    (gxref-create-db (car (project-roots (project-current t)))))
  )
