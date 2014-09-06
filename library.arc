(= rreduce (fn (proc init list)
  (if list
      (proc (car list)
            (rreduce proc init (cdr list)))
      init)))

(= list (fn items
  (rreduce cons nil items)))

(mac def (name args body) (list '= name (list 'fn args body)))

(def abs (x) (if (< x 0) (- 0 x) x))

(def reduce (proc init list)
  (if list
      (reduce proc
             (proc init (car list))
             (cdr list))
      init))

(def reverse (list)
  (reduce (fn (a x) (cons x a)) nil list))

(def unary-map (proc list)
  (rreduce (fn (x rest) (cons (proc x) rest))
         nil
         list))

(def map (proc . arg-lists)
  (if (car arg-lists)
      (cons (apply proc (unary-map car arg-lists))
            (apply map (cons proc
                             (unary-map cdr arg-lists))))
      nil))

(def append (a b) (rreduce cons b a))

(def caar (x) (car (car x)))

(def cadr (x) (car (cdr x)))

(mac and (a b) (list 'if a b nil))
(mac or (a b) (list 'if a t b))

(mac quasiquote (x)
  (if (pair? x)
      (if (eq? (car x) 'unquote)
          (cadr x)
          (if (and (pair? (car x)) (eq? (caar x) 'unquote-splicing))
              (list 'append
                    (cadr (car x))
                    (list 'quasiquote (cdr x)))
              (list 'cons
                    (list 'quasiquote (car x))
                    (list 'quasiquote (cdr x)))))
      (list 'quote x)))

(mac let (defs . body)
  `((fn ,(map car defs) ,@body)
    ,@(map cadr defs)))

(= +
  (let ((old+ +))
    (fn xs (reduce old+ 0 xs))))

(= -
  (let ((old- -))
    (fn (x . xs) (reduce old- x xs))))

(= *
  (let ((old* *))
    (fn xs (reduce old* 1 xs))))

(= /
  (let ((old/ /))
    (fn (x . xs) (reduce old/ x xs))))