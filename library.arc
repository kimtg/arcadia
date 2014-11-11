(mac = args (cons 'assign args))

(= rreduce (fn (proc init list)
  (if list
      (proc (car list)
            (rreduce proc init (cdr list)))
      init)))

(= list (fn args args))

(mac def (name args . body) (list '= name (cons 'fn (cons args body))))

(def no (x) (is x nil))
(def > (x y) (< y x))
(def <= (x y) (no (> x y)))
(def >= (x y) (no (< x y)))

(def isa (x y)
	(is (type x) y))

(def isnt (x y) (no (is x y)))

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

(def len (seq)
  (let i 0
    (if (isa seq 'cons) (do (while seq (++ i) (= seq (cdr seq))) i)
      (do (while (isnt (seq i) 0) (++ i)) i))))

(mac each (var expr . body)
  (w/uniq (seq i)
    `(let ,seq ,expr
      (if (isa ,seq 'cons) (while ,seq (= ,var (car ,seq)) ,@body (= ,seq (cdr ,seq)))
        (let ,i 0 (while (isnt (,seq ,i) 0) (= ,var (,seq ,i)) ,@body (++ ,i)))))))

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

(def join args
  (if (no args)
    nil
    (let a (car args)
      (if (no a)
        (apply join (cdr args))
        (cons (car a) (apply join (cons (cdr a) (cdr args))))))))

(= uniq (let uniq-count 0
  (fn () (sym (string "_uniq" (= uniq-count (+ uniq-count 1)))))))

(mac w/uniq (names . body)
  (if (isa names 'cons)
    `(with ,(apply join (map (fn (x) (list x '(uniq))) names))
       ,@body)
    `(let ,names (uniq) ,@body)))

(mac ++ (place)
  (if (isa place 'cons)
    (w/uniq (a head index)
      (if (is (car place) 'car) `(let ,a ,(cadr place) (scar ,a (+ (car ,a) 1)))
        (if (is (car place) 'cdr) `(let ,a ,(cadr place) (scdr ,a (+ (cdr ,a) 1)))
          `(with (,head ,(car place)
            ,index ,(cadr place))
            (sref ,head (+ (,head ,index) 1) ,index)))))
    `(assign ,place (+ ,place 1))))

(mac -- (place)
  (if (isa place 'cons)
    (w/uniq (a head index)
      (if (is (car place) 'car) `(let ,a ,(cadr place) (scar ,a (- (car ,a) 1)))
        (if (is (car place) 'cdr) `(let ,a ,(cadr place) (scdr ,a (- (cdr ,a) 1)))
          `(with (,head ,(car place)
            ,index ,(cadr place))
            (sref ,head (- (,head ,index) 1) ,index)))))
    `(assign ,place (- ,place 1))))

(def nthcdr (n pair)
	(let i 0
		(while (and (< i n) pair)
			(= pair (cdr pair))
			(++ i)))
	pair)

(def sref (object value index)
	(if (isa object 'cons) (scar (nthcdr index object) value)
		(string-sref object value index)))

(mac = (place value)
  (if (isa place 'cons)
    (if (is (car place) 'car)
      (list 'scar (cadr place) value)
      (if (is (car place) 'cdr)
        (list 'scdr (cadr place) value)
        (list 'sref (car place) value (cadr place))))
    (list 'assign place value)))

(mac when (test . body)
	 (list 'if test (cons 'do body)))

(mac unless (test . body)
  `(if (no ,test) (do ,@body)))

(mac do1 xs `(let it ,(car xs) ,@(cdr xs) it))

(def prn xs (do1 (apply pr xs) (writeb 10)))

(mac for (var init max . body)
  (w/uniq g
  `(let ,g ,max (= ,var ,init)
      (while (<= ,var ,g) ,@body (++ ,var)))))

(def idfn (x) x)

(def number (n)
  "Is 'n' a number?"
  (is (type n) 'num))

(def positive (x)
  (and (number x) (> x 0)))

(mac withs (parms . body)
	 "Like [[with]], but binding for a variable can refer to earlier variables.
For example, (withs (x 1 y (+ x 1))
               (+ x y))
             => 3"
	 (if (no parms)
		 `(do ,@body)
		 `(let ,(car parms) ,(cadr parms)
			   (withs ,(cddr parms) ,@body))))
