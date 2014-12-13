(mac = args (cons 'assign args))

(= rreduce (fn (proc init list)
  (if list
      (proc (car list)
            (rreduce proc init (cdr list)))
      init)))

(= list (fn args args))

(mac def (name args . body) (list '= name (cons 'fn (cons args body))))

(def no (x) (is x nil))

(mac compose args
"Takes a list of functions and returns a function that behaves as if all its
'args' were called in sequence.
For example, this is always true:
  ((compose f g h) a b c) <=> (f (g (h a b c))).
Be wary of passing macros to compose."
  (w/uniq g
    `(fn ,g
       ,((afn (fs)
          (if cdr.fs
            (list car.fs (self cdr.fs))
            `(apply ,(if car.fs car.fs 'idfn) ,g))) args))))

(def complement (f)
"Returns a function that behaves as if the result of calling 'f' was negated.
For example, this is always true:
  ((complement f) a b) <=> (no (f a b))"
  (fn args (no (apply f args))))

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

(def rev (list)
  (reduce (fn (a x) (cons x a)) nil list))

(def map1 (f xs)
"Returns a list containing the result of function 'f' applied to every element of 'xs'."
  (if (no xs)
    nil
    (cons (f (car xs))
          (map1 f (cdr xs)))))

(def map (proc . arg-lists)
  (if (car arg-lists)
      (cons (apply proc (map1 car arg-lists))
            (apply map (cons proc
                             (map1 cdr arg-lists))))
      nil))

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
              (list '+
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
	(isa seq 'string) (do (while (isnt (seq i) #\nul) (++ i)) i)
	(isa seq 'table) (do (maptable (fn (k v) (++ i)) seq) i)
	'else 0)))

(mac each (var expr . body)
     (w/uniq (seq i)
	     `(let ,seq ,expr
		   (if (isa ,seq 'cons) (while ,seq (= ,var (car ,seq)) ,@body (= ,seq (cdr ,seq)))
		       (isa ,seq 'table) (maptable (fn ,var ,@body) ,seq)
		       'else (let ,i 0 (while (isnt (,seq ,i) #\nul) (= ,var (,seq ,i)) ,@body (++ ,i)))))))

(mac do body
	`((fn () ,@body)))

(def pair (xs (o f list))
  "Splits the elements of 'xs' into buckets of two, and optionally applies the
function 'f' to them."
  (if (no xs)
       nil
      (no cdr.xs)
       (list (list car.xs))
      (cons (f car.xs cadr.xs)
            (pair cddr.xs f))))

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

(def iso (x y)
"Are 'x' and 'y' equal-looking to each other? Non-atoms like lists and tables can contain
the same elements (be *isomorphic*) without being identical."
  (or (is x y)
      (and (acons x)
           (acons y)
           (iso (car x) (car y))
           (iso (cdr x) (cdr y)))))

(def <= args
"Is each element of 'args' lesser than or equal to all following elements?"
  (or (no args)
      (no (cdr args))
      (and (no (> (car args) (cadr args)))
           (apply <= (cdr args)))))

(def >= args
"Is each element of 'args' greater than or equal to all following elements?"
  (or (no args)
      (no (cdr args))
      (and (no (< (car args) (cadr args)))
           (apply >= (cdr args)))))

(mac ++ (place (o i 1))
  (if (isa place 'cons)
    (w/uniq (a head index default)
      (if (is (car place) 'car) `(let ,a ,(cadr place) (scar ,a (+ (car ,a) ,i)))
	  (if (is (car place) 'cdr) `(let ,a ,(cadr place) (scdr ,a (+ (cdr ,a) ,i)))
	      (if (cddr place)
		  `(with (,head ,(car place)
				,index ,(cadr place)
				,default ,(cadr (cdr place)))
		     (sref ,head (+ (,head ,index ,default) ,i) ,index))
		  'else
		  `(with (,head ,(car place)
				,index ,(cadr place))
		     (sref ,head (+ (,head ,index) ,i) ,index)))
	  )))
    `(assign ,place (+ ,place ,i))))

(mac -- (place (o i 1))
  (if (isa place 'cons)
    (w/uniq (a head index default)
      (if (is (car place) 'car) `(let ,a ,(cadr place) (scar ,a (- (car ,a) ,i)))
	  (if (is (car place) 'cdr) `(let ,a ,(cadr place) (scdr ,a (- (cdr ,a) ,i)))
	      (if (cddr place)
		  `(with (,head ,(car place)
				,index ,(cadr place)
				,default ,(cadr (cdr place)))
		     (sref ,head (- (,head ,index ,default) ,i) ,index))
		  'else
		  `(with (,head ,(car place)
				,index ,(cadr place))
		     (sref ,head (- (,head ,index) ,i) ,index)))
	  )))
    `(assign ,place (- ,place ,i))))

(def nthcdr (n pair)
	(let i 0
		(while (and (< i n) pair)
			(= pair (cdr pair))
			(++ i)))
	pair)

(def sref (object value index)
  (let type- (type object)
    (if (is type- 'cons) (scar (nthcdr index object) value)
	((if (is type- 'string) string-sref table-sref) object value index))))

; = place value ...
(mac = args
     (cons 'do (map (fn (p) (with (place (car p) value (cadr p))
			      (if (isa place 'cons)
				  (if (is (car place) 'car)
				      (list 'scar (cadr place) value)
				      (if (is (car place) 'cdr)
					  (list 'scdr (cadr place) value)
					  (list 'sref (car place) value (cadr place))))
				  (list 'assign place value))))
		    (pair args))))

(mac when (test . body)
	 (list 'if test (cons 'do body)))

(mac unless (test . body)
  `(if (no ,test) (do ,@body)))

(mac do1 xs `(let it ,(car xs) ,@(cdr xs) it))

(def pr args
	"Prints all its 'args' to screen. Returns the first arg."
  (map1 disp args)
  (car args))

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

(def med (ns (o test >))
	"Returns the median of a list of numbers 'ns' according to the comparison 'test'. Takes the later element for an even-length list."
	((sort test ns) (trunc (/ (len ns) 2))))

(def median (ns)
	"Returns the median of the list (the element at the midpoint of the list when sorted highest-to-lowest). Takes the earlier element for an even-length list."
	((sort < ns) (trunc (/ (len ns) 2))))

(def testify (x)
"Turns an arbitrary value 'x' into a predicate function to compare with 'x'."
  (if (isa x 'fn) x [iso _ x]))

(def reclist (f xs)
"Calls function 'f' with successive [[cdr]]s of 'xs' until one of the calls passes."
  (and xs (or (f xs) (if (acons xs) (reclist f (cdr xs))))))

(mac check (x test (o alt))
"Returns `x' if it satisfies `test', otherwise returns 'alt' (nil if it's not provided)."
  (w/uniq gx
    `(let ,gx ,x
       (if (,test ,gx) ,gx ,alt))))

(def find (test seq)
"Returns the first element of 'seq' that satisfies `test'."
  (let f (testify test)
    (reclist [check (carif _) f] seq)))

(def get (i)
	"Returns a function to pass 'i' to its input.
Useful in higher-order functions, or to index into lists, strings, tables, etc."
	[_ i])

; Syntax expansion is done by reader.
(def ssexpand (symbol) symbol)

(def fill-table (table data)
"Populates 'table' with alternating keys and values in 'data'."
  (do1 table
       (each p pair.data
	     (with (k (car p) v (cadr p))
	       (= table.k v)))))

(def keys (h)
  "Returns list of keys in table 'h'."
  (let r nil
    (maptable (fn (k v) (= r (cons k r))) h) r))

(def vals (h)
  "Returns list of values in table 'h'."
  (let r nil
    (maptable (fn (k v) (= r (cons v r))) h) r))

(def tablist (h)
  "Converts table 'h' into an association list of (key value) pairs. Reverse of [[listtab]]."
  (let r nil
    (maptable (fn p (= r (cons p r))) h) r))

(def listtab (al)
  "Converts association list 'al' of (key value) pairs into a table. Reverse of [[tablist]]."
  (let h (table)
    (map (fn (p) (with (k (car p) v (cadr p)) (= (h k) v)))
         al)
    h))

(mac obj args
"Creates a table out of a list of alternating keys and values."
  `(listtab (list ,@(map (fn (p) (with (k (car p) v (cadr p))
				   `(list ',k ,v)))
                         (pair args)))))

(mac caselet (var expr . args)
"Like [[case]], but 'expr' is also bound to 'var' and available inside the 'args'."
  `(let ,var ,expr
     ,((afn (args)
        (if (no cdr.args)
          car.args
          `(if (is ,var ',car.args)
             ,cadr.args
             ,(self cddr.args)))) args)))

(mac case (expr . args)
"Usage: (case expr test1 then1 test2 then2 ...)
Matches 'expr' to the first satisfying 'test' and runs the corresponding 'then' branch."
  `(caselet ,(uniq) ,expr ,@args))

(mac w/table (var . body)
     "Runs 'body' to add to table 'var' and finally return it."
     `(let ,var (table) ,@body ,var))

(def memtable ((o keys nil) (o val t))
  "Turns a list into a table indicating membership of all elements."
  (w/table tbl
    (each key keys
      (= tbl.key val))))

(def pos (test seq (o start 0))
  "Returns the index of the first element of 'seq' matching 'test', starting
from index 'start' (0 by default)."
  (with (f testify.test seq (coerce seq 'cons))
    ((afn (seq n)
	  (if (no seq)
	      nil
	      (f car.seq)
	      n
	      (self cdr.seq (+ n 1)))) (nthcdr start seq) start)))

(def trues (f xs)
"Returns (map f xs) dropping any nils."
  (and xs
       (iflet fx (f car.xs)
         (cons fx (trues f cdr.xs))
         (trues f cdr.xs))))

(def rem (test seq)
  "Returns all elements of 'seq' except those satisfying 'test'."
  (with (f (testify test) type* (type seq))
    (coerce
     ((afn (s)
	   (if (no s)        nil
	       (f car.s)     (self cdr.s)
	       'else         (cons car.s (self cdr.s)))) (coerce seq 'cons)) type*)))

(def keep (test seq)
  "Returns all elements of 'seq' for which 'test' passes."
  (rem (complement (testify test)) seq))

(def assoc (key al)
  "Finds a (key value) pair in an association list 'al' of such pairs."
  (if (no acons.al) nil
      (and (acons (car al)) (is (caar al) key)) (car al)
      (assoc key (cdr al))))

(def alref (al key)
  "Returns the value of 'key' in an association list 'al' of (key value) pairs"
  (cadr (assoc key al)))

(mac wipe args
"Sets each place in 'args' to nil."
  `(do ,@(map (fn (a) `(= ,a nil)) args)))

(mac set args
  "Sets each place in 'args' to t."
  `(do ,@(map (fn (a) `(= ,a t)) args)))

(mac aif (expr . branches)
"Like [[if]], but also puts the value of 'expr' in variable 'it'."
  `(iflet it ,expr ,@branches))

(mac swap (place1 place2)
  "Exchanges the values of 'place1' and 'place2'."
  (w/uniq g
    `(let ,g ,place1 (= ,place1 ,place2) (= ,place2 ,g))))

(mac rotate places
		 "Like [[swap]] but for more than two places.
For example, after (rotate place1 place2 place3), place3 is moved to place2,
place2 to place1, and place1 to place3."
		 (if (no places) nil
				 (w/uniq g
				 (let binds* nil
					 ((afn (x) (when x (push (list = (car x) (aif (cdr x) (car it) g)) binds*) (self (cdr x)))) places)
					 `(let ,g ,(car places) ,@(rev binds*))))))

(mac zap (op place . args)
  "Replaces 'place' with (op place args...)"
  `(= ,place (,op ,place ,@args)))

(mac push (x place)
  "Adds 'x' to the start of the sequence at 'place'."
  `(= ,place (cons ,x ,place)))

(mac pop (place)
  "Opposite of [[push]]: removes the first element of the sequence at 'place' and returns it."
  `(= ,place (cdr ,place)))

(mac pull (test place)
  "Removes all elements from 'place' that satisfy 'test'."
	`(= ,place (rem ,test ,place)))

(def recstring (test s (o start 0))
"Calls function 'test' with successive characters in string 's' until one of the calls passes."
  ((afn ((o i start))
    (and (< i len.s)
         (or test.i
             (self (+ i 1)))))))

(def some (test seq)
  "Does at least one element of 'seq' satisfy 'test'?"
  (let f testify.test
    (if (isa seq 'string)
	(recstring f:seq seq)
	(reclist f:carif seq))))

(def all (test seq)
  "Does every element of 'seq' satisfy 'test'?"
  (~some (complement (testify test)) seq))

(def adjoin (x xs)
  (if (some x xs)
    xs
    (cons x xs)))

(mac pushnew (x place)
  "Like [[push]] but first checks if 'x' is already present in 'place'."
	`(= ,place (adjoin ,x ,place)))

(mac nor args
  "Computes args until one of them passes, then returns nil.
Returns t if none of the args passes."
  `(no (or ,@args)))

(mac until (test . body)
"Like [[while]], but negates 'test'; loops through 'body' as long as 'test' fails."
  `(while (no ,test) ,@body))

(mac whilet (var test . body)
  "Like [[while]], but successive values of 'test' are bound to 'var'."
  `(let ,var nil
	(while (= ,var ,test) ,@body)))

(mac whiler (var expr end . body)
"Repeatedly binds 'var' to 'expr' and runs 'body' until 'var' matches 'end'."
  (w/uniq gendf
    `(withs (,var nil ,gendf (testify ,end))
       (while (no (,gendf (= ,var ,expr)))
         ,@body))))

(mac loop (start test update . body)
     "Executes start, then executes body repeatedly, checking test before each iteration and executing update afterward."
     `(do ,start
	  (while ,test ,@body ,update)))

(mac accum (accfn . body)
"Runs 'body' (usually containing a loop) and then returns in order all the
values that were called with 'accfn' in the process.
Can be cleaner than map for complex anonymous functions."
  (w/uniq gacc
    `(withs (,gacc nil ,accfn [push _ ,gacc])
       ,@body
       (rev ,gacc))))

(mac drain (expr (o eos nil))
"Repeatedly evaluates 'expr' until it returns 'eos' (nil by default). Returns
a list of the results."
  (w/uniq (gacc gres)
    `(accum ,gacc
       (whiler ,gres ,expr ,eos
         (,gacc ,gres)))))

(mac repeat (n . body)
     "Runs 'body' expression by expression 'n' times."
     (w/uniq g
	     `(for ,g 1 ,n ,@body)))

(mac forlen (var s . body)
     "Loops through the length of sequence 's', binding each element to 'var'."
     `(for ,var 0 (- (len ,s) 1) ,@body))

(mac noisy-each (n var val . body)
"Like [[each]] but print a progress indicator every 'n' iterations."
  (w/uniq (gn gc)
    `(with (,gn ,n ,gc 0)
       (each ,var ,val
         (when (multiple (++ ,gc) ,gn)
           (pr ".")
           (flushout)
           )
         ,@body)
       (prn)
       (flushout))))

(mac on (var s . body)
"Like [[each]], but also maintains a variable calles 'index' counting the iterations."
  (if (is var 'index)
    (err "Can't use index as first arg to on.")
    (w/uniq gs
      `(let ,gs ,s
         (forlen index ,gs
           (let ,var (,gs index)
             ,@body))))))

(mac ontable (k v tab . body)
     "Iterates over the table tab, assigning k and v each key and value."
     `(maptable (fn (,k ,v) ,@body) ,tab))

(def empty (seq)
  "Is 'seq' an empty container? Usually checks 'seq's [[len]]."
  (iso 0 len.seq))

(def orf fns
"Returns a function which calls all the functions in 'fns' on its args, and
[[or]]s the results. ((orf f g) x y) <=> (or (f x y) (g x y))"
  (fn args
    ((afn ((o fs fns))
      (and fs
           (or (apply car.fs args)
               (self cdr.fs)))))))

(def andf fns
"Returns a function which calls all the functions in 'fns' on its args, and
[[and]]s the results. For example, ((andf f g) x y) <=> (and (f x y) (g x y)).
Simple syntax: f&g <=> (andf f g)"
  (fn args
    ((afn ((o fs fns))
      (if no.fs          t
          (no cdr.fs)    (apply car.fs args)
          'else          (and (apply car.fs args)
                              (self cdr.fs)))))))

(def atend (i s)
"Is index 'i' at or past the end of sequence 's'?"
  (>= i (- len.s 1)))

(mac aand args
"Like [[and]], but each expression in 'args' can access the result of the
previous one in variable 'it'."
  (if (no args)
       t
      (no (cdr args))
       (car args)
      `(let it ,(car args) (and it (aand ,@(cdr args))))))

(def dotted (x)
"Is 'x' an _improper_ list terminating in something other than nil?
Name comes from (cons 1 2) being printed with a dot: (1 . 1)."
  (aand acons.x
        cdr.x
        ((orf ~acons dotted) it)))

(mac conswhen (f x y)
"Adds 'x' to the front of 'y' if 'x' satisfies test 'f'."
  (w/uniq (gf gx)
   `(with (,gf ,f  ,gx ,x)
      (if (,gf ,gx) (cons ,gx ,y) ,y))))

(def consif (x xs)
  "Like [[cons]] on 'x' and 'xs' unless 'x' is nil."
  (if x (cons x xs) xs))

(def last (xs)
  "Returns the last element of 'xs'."
  (if (cdr xs)
    (last (cdr xs))
    (car xs)))

(def flat x
  "Flattens a list of lists."
  ((afn ((o x x) (o acc nil))
    (if no.x        acc
        (~acons x)  (cons x acc)
        'else       (self car.x (self cdr.x acc))))))

(def caris (x val)
  (and (acons x) (is (car x) val)))

; common uses of map
(def mappend (f . args)
"Like [[map]] followed by append."
  (apply + (apply + (map [map f _] args))))

(def range-bounce (i max)
"Munges index 'i' in slices of a sequence of length 'max'. First element starts
 at index 0. Negative indices count from the end. A nil index denotes the end."
  (if (no i)  max
      (< i 0)  (+ max i)
      (>= i max) max
      'else  i))

(def cut (seq start (o end))
"Extract a chunk of 'seq' from index 'start' (inclusive) to 'end' (exclusive). 'end'
can be left out or nil to indicate everything from 'start', and can be
negative to count backwards from the end."
  (firstn (- (range-bounce end len.seq)
             start)
          (nthcdr start seq)))

(def split (seq pos)
  "Partitions 'seq' at index 'pos'."
	(list (cut seq 0 pos) (cut seq pos)))

; Generalization of pair: (tuples x) = (pair x)
(def tuples (xs (o n 2))
"Splits 'xs' up into lists of size 'n'. Generalization of [[pair]]."
  (if (no xs)
    nil
    (cons (firstn n xs)
          (tuples (nthcdr n xs) n))))

(def copylist (x) x)

(def inc (x (o n 1))
  (coerce (+ (coerce x 'int) n) (type x)))

(def range (start end)
"Returns the list of integers from 'start' to 'end' (both inclusive)."
  (if (> start end)
    nil
    (cons start (range (inc start) end))))

(mac n-of (n expr)
  "Runs 'expr' 'n' times, and returns a list of the results."
  (w/uniq ga
    `(let ,ga nil
       (repeat ,n (push ,expr ,ga))
       (rev ,ga))))

(def counts (seq (o tbl (table)))
"Returns a table with counts of each unique element in 'seq'."
  (let ans tbl
    (each x seq
	  (++ (ans x 0)))
    ans))

(def compare (comparer scorer)
  "Creates a function to score two args using 'scorer' and compare them using
'comparer'. Often passed to [[sort]]."
  (fn (x y) (comparer scorer.x scorer.y)))

(def commonest (seq)
  "Returns the most common element of 'seq' and the number of times it occurred
in 'seq'."
  (withs (counts* (counts seq)
          best* (best (compare > counts*) seq)) 
    (list best* (counts* best* 0))))

(def retrieve (n f xs)
"Returns the first 'n' elements of 'xs' that satisfy 'f'."
  (if (no n)                 (keep f xs)
      (or no.xs (<= n 0))    nil
      (f car.xs)             (cons car.xs (retrieve (- n 1) f cdr.xs))
                             (retrieve n f cdr.xs)))
(def most (f seq)
"Like [[best]], but function 'f' is a scorer for each element rather than a
comparator between elements."
  (if seq
    (withs (wins (car seq) topscore (f wins))
      (each elt (cdr seq)
        (let score (f elt)
          (if (> score topscore) (= wins elt topscore score))))
      wins)))

(def mem (test seq)
"Returns suffix of 'seq' after the first element to satisfy 'test'.
This is the most reliable way to check for presence, even when searching for nil."
  (let f (testify test)
    (reclist [if (f:carif _) _] seq)))

(def insert-sorted (test elt seq)
"Inserts 'elt' into a sequence 'seq' that is assumed to be sorted by 'test'."
  (if (no seq)
       (list elt)
      (test elt car.seq)
       (cons elt seq)
      'else
      (cons car.seq (insert-sorted test elt cdr.seq))))

(mac insort (test elt seq)
  "Like [[insert-sorted]] but modifies 'seq' in place'."
  `(zap [insert-sorted ,test ,elt _] ,seq))

(def reinsert-sorted (test elt seq)
  (if (no seq)
       (list elt)
      (is elt car.seq)
       (reinsert-sorted test elt cdr.seq)
      (test elt car.seq)
       (cons elt (rem elt seq))
      'else
       (cons car.seq (reinsert-sorted test elt cdr.seq))))

(mac insortnew (test elt seq)
  "Like [[insort]], but only inserts 'elt' if it doesn't exist."
  `(zap [reinsert-sorted ,test ,elt _] ,seq))

(def bestn (n f seq)
  "Returns a list of the top 'n' elements of 'seq' ordered by 'f'."
  (firstn n (sort f seq)))

(def count (test x)
"Returns the number of elements of 'x' that pass 'test'."
  (with (n 0 testf testify.test)
    (each elt x
      (if testf.elt ++.n))
    n))

(def union (f xs ys)
"Merges 'xs' and 'ys', while filtering out duplicates using 'f'. Ordering is
not preserved."
  (+ xs (rem (fn (y) (some [f _ y] xs))
             ys)))
