(mac = args (cons 'set args))

(= rreduce (fn (proc init list)
  (if list
      (proc (car list)
            (rreduce proc init (cdr list)))
      init)))

(= list (fn args args))

(mac def (name args . body) (list '= name (cons 'fn (cons args body))))

(def no (x) (is x nil))

(def isa (x y)
	(is (type x) y))

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
      (if (is (car x) 'unquote)
          (cadr x)
          (if (and (pair? (car x)) (is (caar x) 'unquote-splicing))
              (list 'append
                    (cadr (car x))
                    (list 'quasiquote (cdr x)))
              (list 'cons
                    (list 'quasiquote (car x))
                    (list 'quasiquote (cdr x)))))
      (list 'quote x)))

(mac let (sym def . body)
	`((fn (,sym) ,@body) ,def))

(def len (lst)
	(if (no lst) 0
		(+ 1 (len (cdr lst)))))
		
(mac do body
	`((fn () ,@body)))

(mac ++ (a) `(= ,a (+ ,a 1)))
(mac -- (a) `(= ,a (- ,a 1)))

(def nthcdr (n pair)
	(let i 0
		(while (and (< i n) pair)
			(= pair (cdr pair))
			(++ i)))
	pair)

(def nth (n pair) (car (nthcdr n pair)))
(def setnth (n a value)
	(if (isa a 'pair) (scar (nthcdr n pair) value)
		(string-setnth n a value)))

(mac = (place value)
  (if (pair? place)
    (if (is (car place) 'car)
      (list 'scar (cadr place) value)
      (if (is (car place) 'cdr)
        (list 'scdr (cadr place) value)
        (list 'setnth (cadr place) (car place) value)))
    (list 'set place value)))
