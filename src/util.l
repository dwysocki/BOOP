(defun partial (f &rest args)
  (lambda (&rest args*)
    (apply f (append args args*))))

(defun compose (&rest functions)
  "Compose FUNCTIONS right-associatively, returning a function"
  #'(lambda (x)
      (reduce #'funcall functions
              :initial-value x
              :from-end t)))
