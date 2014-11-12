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

(mac and2 (a b) (list 'if a b nil))
(mac or (a b) (list 'if a t b))

(mac quasiquote (x)
  (if (isa x 'cons)
      (if (is (car x) 'unquote)
          (cadr x)
          (if (and2 (isa (car x) 'cons) (is (caar x) 'unquote-splicing))
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

(mac and args
"Stops at the first argument to fail (return nil). Returns the last argument before stopping."
  (if args
    (if (cdr args)
      `(if ,(car args) (and ,@(cdr args)))
      (car args))
    t))

(mac or args
"Stops at the first argument to pass, and returns its result."
  (and args
       (w/uniq g
         `(let ,g ,(car args)
            (if ,g ,g
              (or ,@(cdr args)))))))

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

(def even (n)
"Is n even?"
  (is (mod n 2) 0))

(def odd (n)
"Is n odd?"
  (no (even n)))

(def round (n)
"Approximates a fractional value to the nearest even integer.
Negative numbers are always treated exactly like their positive variants
barring the sign."
  (withs (base (trunc n) rem (abs (- n base)))
    (if (> rem 0.5) ((if (> n 0) + -) base 1)
        (< rem 0.5) base
        (odd base)  ((if (> n 0) + -) base 1)
		base)))

(def roundup (n)
"Like [[round]] but halves are rounded up rather than down."
  (withs (base (trunc n) rem (abs (- n base)))
    (if (>= rem 0.5)
      ((if (> n 0) + -) base 1)
      base)))

(def nearest (n quantum)
  "Like [[round]] but generalized to arbitrary units."
  (* (roundup (/ n quantum)) quantum))

(def avg (ns)
  "Returns the arithmetic mean of a list of numbers 'ns'."
  (/ (apply + ns) (len ns)))

(def multiple (x y)
  "Is 'x' a multiple of 'y'?"
  (is 0 (mod x y)))

(def carif (x)
  "Returns the first element of the given list 'x', or just 'x' if it isn't a list."
  (if (is (type x) 'cons) (car x) x))

(mac iflet (var expr . branches)
"If 'expr' is not nil, binds 'var' to it before running the first branch.
Can be given multiple alternating test expressions and branches. The first
passing test expression is bound to 'var' before running its corresponding branch.

For examples, see [[aif]]."
  (if branches
    (w/uniq gv
      `(let ,gv ,expr
         (if ,gv
           (let ,var ,gv
             ,(car branches))
           ,(if (cdr branches)
              `(iflet ,var ,@(cdr branches))))))
    expr))

(mac whenlet (var expr . body)
	 "Like [[when]] but also puts the value of 'expr' in 'var' so 'body' can access it."
	 `(iflet ,var ,expr (do ,@body)))

(def best (f seq)
  "Maximizes comparator function 'f' throughout seq."
  (whenlet wins (carif seq)
		   (each elt (cdr seq)
				 (if (f elt wins)
					 (= wins elt)))
		   wins))

(def max args
  "Returns the greatest of 'args'."
  (best > args))

(def min args
  "Returns the least of 'args'."
  (best < args))

(def firstn (n xs)
	"Returns the first 'n' elements of 'xs'."
  (if (no n)            xs
      (and (> n 0) xs)  (cons (car xs) (firstn (- n 1) (cdr xs)))
			nil))

(mac rfn (name parms . body)
"Like [[fn]] but permits the created function to call itself recursively as the given 'name'."
  `(let ,name nil
     (assign ,name (fn ,parms ,@body))))

(mac afn (parms . body)
"Like [[fn]] and [[rfn]] but the created function can call itself as 'self'"
  `(rfn self ,parms ,@body))

; Destructive stable merge-sort, adapted from slib and improved
; by Eli Barzilay for MzLib; re-written in Arc.

(def mergesort (less? lst)
  (with (n (len lst))
    (if (<= n 1) lst
        ((rfn recur (n)
           (if (> n 2)
                ; needs to evaluate L->R
                (withs (j (/ (if (even n) n (- n 1)) 2) ; faster than round
                        a (recur j)
                        b (recur (- n j)))
                  (merge less? a b))
               ; the following case just inlines the length 2 case,
               ; it can be removed (and use the above case for n>1)
               ; and the code still works, except a little slower
               (is n 2)
                (with (x (car lst) y (cadr lst) p lst)
                  (= lst (cddr lst))
                  (when (less? y x) (scar p y) (scar (cdr p) x))
                  (scdr (cdr p) nil)
                  p)
               (is n 1)
                (with (p lst)
                  (= lst (cdr lst))
                  (scdr p nil)
                  p)
               nil)) n))))

; Also by Eli.

(def merge (less? x y)
  (if (no x) y
      (no y) x
      (let lup nil
        (assign lup
                (fn (r x y r-x?) ; r-x? for optimization -- is r connected to x?
                  (if (less? (car y) (car x))
                    (do (if r-x? (scdr r y))
                        (if (cdr y) (lup y x (cdr y) nil) (scdr y x)))
                    ; (car x) <= (car y)
                    (do (if (no r-x?) (scdr r x))
                        (if (cdr x) (lup x (cdr x) y t) (scdr x y))))))
        (if (less? (car y) (car x))
          (do (if (cdr y) (lup y x (cdr y) nil) (scdr y x))
              y)
          ; (car x) <= (car y)
          (do (if (cdr x) (lup x (cdr x) y t) (scdr x y))
              x)))))

(def acons (x)
"Is 'x' a non-nil list?"
  (is (type x) 'cons))

(def alist (x)
"Is 'x' a (possibly empty) list?"
(or (no x) (acons x)))

(mac in (x . choices)
"Does 'x' match one of the given 'choices'?"
  (w/uniq g
    `(let ,g ,x
       (or ,@(map1 (fn (c) `(is ,g ,c))
                   choices)))))

(def atom (x)
"Is 'x' a simple type? (i.e. not list, table or user-defined)"
  (in (type x) 'int 'num 'sym 'char 'string))

(def copy (x)
"Creates a deep copy of 'x'. Future changes to any part of 'x' are guaranteed
to be isolated from the copy."
  (if (atom x)
    x
    (cons (copy (car x))
          (copy (cdr x)))))

; Use mergesort on assumption that mostly sorting mostly sorted lists
(def sort (test seq)
"Orders a list 'seq' by comparing its elements using 'test'."
  (if (alist seq)
    (mergesort test (copy seq))
    (coerce (mergesort test (coerce seq 'cons)) (type seq))))

(def med (ns . test)
	"Returns the median of a list of numbers 'ns' according to the comparison 'test'."
	(= test (if (no test) > (car test)))
	((sort test ns) (round (/ (len ns) 2))))
