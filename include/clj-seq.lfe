(defmacro get-in args
  (let* ((rargs (lists:reverse args))
         (data (car rargs))
         (keys (lists:reverse (cdr rargs))))
    `(apply #'clj-seq:get-in/2 (list ,data (list ,@keys)))))

(defun loaded-seq ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
