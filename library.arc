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

(def map1 (proc list)
  (rreduce (fn (x rest) (cons (proc x) rest))
         nil
         list))

(def map (proc . arg-lists)
  (if (car arg-lists)
      (cons (apply proc (map1 car arg-lists))
            (apply map (cons proc
                             (map1 cdr arg-lists))))
      nil))

(def append (a b) (rreduce cons b a))

(def caar (x) (car (car x)))
(def cadr (x) (car (cdr x)))
(def cddr (x) (cdr (cdr x)))

(mac and (a b) (list 'if a b nil))
(mac or (a b) (list 'if a t b))

(mac quasiquote (x)
  (if (isa x 'cons)
      (if (is (car x) 'unquote)
          (cadr x)
          (if (and (isa (car x) 'cons) (is (caar x) 'unquote-splicing))
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

(def pair (xs)
  (if (no xs)
       nil
      (no (cdr xs))
       (list (list (car xs)))
      (cons (list (car xs) (cadr xs))
            (pair (cddr xs)))))

(mac with (parms . body)
  `((fn ,(map1 car (pair parms))
     ,@body)
    ,@(map1 cadr (pair parms))))

(mac ++ (place)
  (if (isa place 'cons)
    (if (is (car place) 'car) `(let _a ,(cadr place) (scar _a (+ _a 1)))
      (if (is (car place) 'cdr) `(let _a ,(cadr place) (scdr _a (+ _a 1)))
        `(with (_head ,(car place)
          _index ,(cadr place))
          (sref _head (+ (_head _index) 1) _index))))
    `(set ,place (+ ,place 1))))

(mac -- (place)
  (if (isa place 'cons)
    (if (is (car place) 'car) `(let _a ,(cadr place) (scar _a (- _a 1)))
      (if (is (car place) 'cdr) `(let _a ,(cadr place) (scdr _a (- _a 1)))
        `(with (_head ,(car place)
          _index ,(cadr place))
          (sref _head (- (_head _index) 1) _index))))
    `(set ,place (- ,place 1))))

(def nthcdr (n pair)
	(let i 0
		(while (and (< i n) pair)
			(= pair (cdr pair))
			(++ i)))
	pair)

(def sref (object value index)
	(if (isa object 'cons) (scar (nthcdr index object) value)
		(string-setnth index object value)))

(mac = (place value)
  (if (isa place 'cons)
    (if (is (car place) 'car)
      (list 'scar (cadr place) value)
      (if (is (car place) 'cdr)
        (list 'scdr (cadr place) value)
        (list 'sref (car place) value (cadr place))))
    (list 'set place value)))

(mac when (test . body)
	 (list 'if test (cons 'do body)))

(mac do1 xs `(let it ,(car xs) ,@(cdr xs) it))

(def prn xs (do1 (apply pr xs) (writeb 10)))
