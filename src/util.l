(defun partial (f &rest args)
  (lambda (&rest args*)
    (apply f (append args args*))))
