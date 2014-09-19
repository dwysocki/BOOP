(load "util.l")

(defun evalexp (bindings exp)
  (simplify (bind-values bindings exp)))

(defun eval-basic (exp)
  (apply (car exp)
         (cdr exp)))

(defun bind-values (bindings exp)
  ;; for now just returns expression ;;
  ;; fix this later ;;
  exp)

;; SOMETHING IS WRONG WITH THIS ;;
(defun simplify (exp)
  (if (and (listp exp) (not (null exp)))
    (apply (car exp)
           (mapcar #'simplify (cdr exp)))
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

;; Back up built-in boolean functions
(defconstant *cl-and* (fdefinition 'and))
(defconstant *cl-or*  (fdefinition 'or))
(defconstant *cl-eql* (fdefinition 'eql))
(defconstant *cl-not* (fdefinition 'not))

;; Shadow built-in boolean functions
(mapcar #'shadow '(and or eql not))


(defun and (&rest args)
  (cond
   ; if NIL is in the arguments, the expression evaluates to NIL
   ((member NIL args)  NIL)
   ; if all of the args are the same, return one of the args
   ((apply #'eql args) (car args))
   ; return the first symbol that is not T, or T if none exist
   (T                  (let ((symbol (first (remove-if #'true? args))))
                         (if symbol
                           symbol
                           T)))))

(defun or (&rest args)
  (if (member T args)
    T
    (first (remove-if #'false? args))))

(defun eql (&rest args)
  (apply #'equal args))

(defun not (exp)
  (cond
   ((true? exp)   NIL)
   ((false? exp)  T)
   ((andexp? exp) (apply #'orexp
                         (mapcar #'notexp
                                 (cdr exp))))
   ((orexp? exp)  (apply #'andexp
                         (mapcar #'notexp
                                 (cdr exp))))
   (T             (notexp exp))))
