(load "util.l")

(defun evalexp (bindings exp)
  (simplify (bind-values bindings exp)))

(defun bind-values (bindings exp)
  (if (null bindings)
    exp
    (let* ((current-bindings (car bindings))
	   (old              (first  current-bindings))
	   (new              (second current-bindings)))
      (bind-values (cdr bindings)
		   (subst new old exp)))))

;; SOMETHING IS WRONG WITH THIS ;;
(defun simplify (exp)
  (if (and (listp exp) exp)
    (let* ((operator            (car exp))
	   (operands            (cdr exp))
	   (simplified-operands (mapcar #'simplify operands))
	   (simplified-exp      (cons operator simplified-operands))
	   (simplifier          (case operator
				  ('and #'simplify-and)
				  ('or  #'simplify-or)
				  ('eql #'simplify-eql)
				  ('not #'simplify-not))))
      (funcall simplifier simplified-exp))
    exp))


;; Expression builder functions
(defun andexp (e1 e2)
  (list 'and e1 e2))

(defun orexp  (e1 e2)
  (list 'or e1 e2))

(defun eqlexp (e1 e2)
  (list 'eql e1 e2))

(defun notexp (e1)
  (list 'not e1))

;; Predicates
(defun true? (x)
  (equal x T))

(defun false? (x)
  (equal x NIL))

(defun andexp? (exp)
  (when (listp exp)
    (equal 'and (car exp))))

(defun orexp? (exp)
  (when (listp exp)
    (equal 'or (car exp))))

(defun eqlexp? (exp)
  (when (listp exp)
    (equal 'eql (car exp))))

(defun notexp? (exp)
  (when (listp exp)
    (equal 'not (car exp))))

;; Simplifiers
(defun simplify-and (exp)
  (let ((operands (cdr exp)))
    (cond
     ((member nil operands)    nil)
     ((apply #'equal operands) (car operands))
     (T                        exp))))

(defun simplify-or (exp)
  (let ((operands (cdr exp)))
    (if (member T operands)
      T
      (let* ((non-false-operands (remove-if #'false? operands))
	     (unique-operands    (remove-duplicates non-false-operands)))
	(case (length unique-operands)
	  (0         nil)
	  (1         (car unique-operands))
	  (otherwise exp))))))

(defun simplify-eql (exp)
  (let ((operands (cdr exp)))
    (apply #'eql operands)))

(defun simplify-not (exp)
  (let ((operand (second exp)))
    (cond
     ((true? operand)   NIL)
     ((false? operand)  T)
     ((andexp? operand) (apply #'orexp
                               (mapcar (compose #'simplify-not #'notexp)
                                       (cdr operand))))
     ((orexp? operand)  (apply #'andexp
                               (mapcar (compose #'simplify-not #'notexp)
                                       (cdr operand))))
     ((notexp? operand) (second operand))
     (T                 exp))))
