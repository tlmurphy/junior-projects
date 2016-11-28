; Assignment 3
; CS 403 - Programming Languages
; Written by Trevor Murphy

(define (author)
    (println "AUTHOR: Trevor Murphy tmurphy2@crimson.ua.edu"))

; For testing purposes
(define (exprTest # $expr target)
    (define result (catch (eval $expr #)))
        (if (error? result)
            (println $expr " is EXCEPTION: " (result'value)
                " (it should be " target ")")
            (println $expr " is " result
                " (it should be " target ")")))


;===================================Task 1======================================

(define (nonlocals func)
    (define locals (get 'parameters func))
    (define body (get 'code func))

    (define (iterBody body newBody)
        (cond
            ((null? body) newBody)
            ((pair? (car body)) (iterBody (cdr body) (append newBody (iterBody (car body) '()))))
            (else
                (if (and (not (in? (car body) locals)) (equal? (type (car body)) 'SYMBOL))
                    (iterBody (cdr body) (cons (car body) newBody))
                    (iterBody (cdr body) newBody)))))

    ; Old version of in?
    (define (old-in? a list)
        (if (null? list)
            #f
            (if (equal? a (car list))
                #t
                (in? a (cdr list)))))

    ; I'm getting better with recursion
    (define (in? a list)
        (if (not (null? list))
            (or (equal? a (car list)) (in? a (cdr list)))))


    ; There shouldn't be any duplicates
    (define (removeDuplicates list newList)
        (if (null? list)
            newList
            (if (not (in? (car list) newList))
                (removeDuplicates (cdr list) (append newList (cons (car list) nil)))
                (removeDuplicates (cdr list) newList))))

    (removeDuplicates (iterBody body '()) '()))

(define (run1)
    (define (square x) (* x x))
    (define (test1 x y z)
        (if (> x y)
            (println "AYY")
            (println "OH"))
        (+ z 1))
    (define (test2 x)
        (define zz (+ x 1))
        (println "HEY")
        (- zz 10))
    (inspect (nonlocals square))
    (inspect (nonlocals test2)))


;===================================Task 2======================================

(define (replace func sym repl)
    (define body (cdr (get 'code func)))

    (define (iterBody body newBody)
        (cond
            ((null? body) newBody)
            ((pair? (car body)) (iterBody (cdr body) (append newBody (list (iterBody (car body) '())))))
            (else
                (if (equal? (car body) sym)
                    (iterBody (cdr body) (append newBody (list repl)))
                    (iterBody (cdr body) (append newBody (list (car body))))))))

    (cons 'begin (iterBody body '())))

(define (run2)
    (define (fib n)
        (if (<= n 2)
            1
            (+ (fib (- n 1)) (fib (- n 2)))))
    (define (square x) (* x x))
    (define (test1 x y z)
        (if (> x y)
            (println "AYY")
            (println "OH"))
        (+ z 1))
    (define (test2 x y z)
        (if (> z 1)
            (+ z 1)))

    (inspect (test1 1 2 3))
    (println (replace test1 '+ *))
    (set 'code (replace test1 '+ *) test1)
    (inspect (test1 1 2 3))
    (inspect (fib 10))
    ; do something here
    (inspect (fib 10)))


;===================================Task 3======================================

(define (node value parent)
    (define (helper val l r p lHeight rHeight height) this)
    (helper value nil nil parent 0 0 1))

; Queue from Lusth's notes...
(define (Queue)
    (define front (list 'head))
    (define back nil)

    (define (this msg . args)
        (cond
            ((eq? msg 'enqueue) (apply enqueue args))
            ((eq? msg 'dequeue) (apply dequeue args))
            ((eq? msg 'empty?) (apply empty? args))
            (else (error "queue message not understood: " msg))
            )
        )
    (define (enqueue x) ; add to the back
        (set-cdr! back (list x))
        (set! back (cdr back))
        )
    (define (dequeue) ; remove from the front
        ; user is responsible ensuring queue is non empty
        (define tmp (cadr front))
        (set-cdr! front (cddr front))
        (if (null? (cdr front))
            (set! back front)
            )
        tmp
        )
    (define (empty?)
        (eq? (cdr front) nil)
        )

    (set! back front)
    this
    )

(define (avl)

    (define (helper s root)

        ; Methods
        (define (size) s)

        (define (max x y)
            (if (> x y)
                x
                y))

        (define (setBalance x)
            (if (null? (x 'l))
                (set 'lHeight 0 x)
                (set 'lHeight ((x 'l) 'height) x))
            (if (null? (x 'r))
                (set 'rHeight 0 x)
                (set 'rHeight ((x 'r) 'height) x))
            (set 'height (+ (max (x 'lHeight) (x 'rHeight)) 1) x)
            (if (not (null? (x 'p)))
                (if (isRight? x)
                    (set 'rHeight (x 'height) (x 'p))
                    (set 'lHeight (x 'height) (x 'p)))))

        (define (sibling x)
            (if (eq? x ((x 'p) 'l))
                ((x 'p) 'r))
            (if (eq? x ((x 'p) 'r))
                ((x 'p) 'l)))

        (define (bFactor x)
            (- (x 'lHeight) (x 'rHeight)))

        (define (isRight? x)
            (eq? x ((x 'p) 'r)))

        (define (isLeft? x)
            (eq? x ((x 'p) 'l)))

        (define (favorSibling? x)
            (if (isRight? x)
                (cond
                    ((= 1 (bFactor (x 'p))) #t)
                    ((= (- 1) (bFactor (x 'p))) #f)
                    (else #f))
                (cond
                    ((= 1 (bFactor (x 'p))) #f)
                    ((= (- 1) (bFactor (x 'p))) #t)
                    (else #f))))

        (define (isBalanced? x)
            (or (= 1 (bFactor x)) (= (- 1) (bFactor x)) (= 0 (bFactor x))))

        (define (favoriteChild x)
            (cond
                ((= 1 (bFactor x)) (x 'l))
                ((= (- 1) (bFactor x)) (x 'r))
                (else nil)))

        (define (linear? p x y)
            (or (and (eq? (p 'r) x) (eq? (x 'r) y))
                (and (eq? (p 'l) x) (eq? (x 'l) y))))

        (define (rotateYX y x)
            (if (isLeft? y)
                (rotateRight y x)
                (rotateLeft y x)))

        (define (rotateYP y p)
            (if (isLeft? y)
                (rotateRight y p)
                (rotateLeft y p)))

        (define (rotateXP x p)
            (if (isRight? x)
                (rotateLeft x p)
                (rotateRight x p)))

        (define (rotateLeft a b)
            (define left (a 'l))
            (define p (b 'p))
            (if (null? p)
                (set! root a)
                (if (isLeft? b)
                    (set 'l a p)
                    (set 'r a p)))
            (set 'l b a)
            (set 'r left b)
            (if (not (null? left))
                (set 'p b left))
            (set 'p a b)
            (set 'p p a))

        (define (rotateRight a b)
            (define right (a 'r))
            (define p (b 'p)) ; 12
            (if (null? p)
                (set! root a)
                (if (isLeft? b)
                    (set 'l a p)
                    (set 'r a p)))
            (set 'r b a)
            (set 'l right b)
            (if (not (null? right))
                (set 'p b right))
            (set 'p a b)
            (set 'p p a))

        (define (fix x)
            (if (eq? x root)
                (set! root x)
                (cond
                    ((favorSibling? x) (setBalance (x 'p)))
                    ((isBalanced? (x 'p))
                        (setBalance (x 'p))
                        (set! x (x 'p))
                        (fix x))
                    (else
                        (define y (favoriteChild x))
                        (define p (x 'p))
                        (if (and (not (null? y)) (not (linear? p x y)))
                            (begin
                                (rotateYX y x)
                                (rotateYP y p)
                                (setBalance x)
                                (setBalance p)
                                (setBalance y))
                            (begin
                                (rotateXP x p)
                                (setBalance p)
                                (setBalance x)))))))

        (define (insert n)
            (++ s)
            (define (iter p)
                (if (null? p)
                    (set! root (node n nil))
                    (if (< n (p 'val))
                        (if (null? (p 'l))
                            (begin
                                (set 'l (node n p) p)
                                (set 'lHeight 1 p)
                                (fix (p 'l)))
                            (iter (p 'l)))
                        (if (null? (p 'r))
                            (begin
                                (set 'r (node n p) p)
                                (set 'rHeight 1 p)
                                (fix (p 'r)))
                            (iter (p 'r))))))
            (iter root))

        (define (find n)
            (define (iter p)
                (cond
                    ((null? p) #f)
                    ((= (p 'val) n) #t)
                    ((< n (p 'val)) (iter (p 'l)))
                    (else
                        (iter (p 'r)))))
            (iter root))

        (define (statistics)
            (define (levelOrder q)
                (if ((q 'empty?))
                    nil
                    (begin
                        (define node ((q 'dequeue)))
                        (print (node 'val) ":" (bFactor node) " ")
                        (if (not (null? (node 'l)))
                            ((q 'enqueue) (node 'l)))
                        (if (not (null? (node 'r)))
                            ((q 'enqueue) (node 'r)))
                        (levelOrder q))))
            (define newQueue (Queue))
            ((newQueue 'enqueue) root)
            (levelOrder newQueue)
            (println))

        this)
    (helper 0 nil))

(define (run3)
    (define t (avl))
    ((t 'insert) 3)
    ((t 'insert) 4)
    ((t 'insert) 5)
    ((t 'insert) 1)
    ((t 'insert) 12)
    ((t 'insert) 55)
    ((t 'insert) 6)
    ((t 'insert) 90)
    ((t 'insert) 122)
    ((t 'insert) 54)
    ((t 'insert) 32)
    ((t 'insert) 15)
    ((t 'insert) 905)
    ((t 'insert) 67)
    ((t 'insert) 322)
    ((t 'insert) 78)
    ((t 'insert) 45)
    ((t 'insert) 46)
    ((t 'insert) 47)
    ((t 'insert) 48)
    (inspect ((t 'find) 90))    ; should return #t
    (inspect ((t 'find) 5))     ; should return #t
    (inspect ((t 'find) 322))   ; should return #t
    (inspect ((t 'find) 7))     ; should return #f
    (inspect ((t 'size)))       ; should return 20
    ((t 'statistics)))


;===================================Task 4======================================

(define true #t)
(define false #f)

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (for-each-except exception procedure list)
    (define (loop items)
      (cond ((null? items) 'done)
            ((eq? (car items) exception) (loop (cdr items)))
            (else (procedure (car items))
                  (loop (cdr items)))))
    (loop list))

(define (memq item x)
    (cond
        ((null? x) false)
        ((eq? item (car x)) true)
        (else
            (memq item (cdr x)))))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
    (define (process-new-value)
      (cond ((or (and (has-value? m1) (= (get-value m1) 0))
                 (and (has-value? m2) (= (get-value m2) 0)))
             (set-value! product 0 me))
            ((and (has-value? m1) (has-value? m2)) ; m1 * m2 = product
             (set-value! product
                         (* (get-value m1) (get-value m2))
                         me))
            ((and (has-value? product) (has-value? m1)) ; product / m1 = m2
             (set-value! m2
                         (/ (real (get-value product)) (get-value m1))
                         me))
            ((and (has-value? product) (has-value? m2)) ; product / m2 = m1
             (set-value! m1
                         (/ (real (get-value product)) (get-value m2))
                         me))))
    (define (process-forget-value)
      (forget-value! product me)
      (forget-value! m1 me)
      (forget-value! m2 me)
      (process-new-value))
    (define (me request)
      (cond ((eq? request 'I-have-a-value)
             (process-new-value))
            ((eq? request 'I-lost-my-value)
             (process-forget-value))
            (else
             (error "Unknown request - MULTIPLIER" request))))
    (connect m1 me)
    (connect m2 me)
    (connect product me)
    me)



(define (divider m1 m2 result)
    (define (process-new-value)
      (cond ((or (and (has-value? m1) (= (get-value m1) 0))
                 (and (has-value? m2) (= (get-value m2) 0)))
             (set-value! result 0 me))
            ((and (has-value? m1) (has-value? m2)) ; m1 / m2 = result
             (set-value! result
                         (/ (real (get-value m1)) (get-value m2))
                         me))
            ((and (has-value? result) (has-value? m1)) ; m1 / result = m2
             (set-value! m2
                         (/ (real (get-value m1)) (get-value result))
                         me))
            ((and (has-value? result) (has-value? m2)) ; m2 * result = m1
             (set-value! m1
                         (* (get-value result) (get-value m2))
                         me))))
    (define (process-forget-value)
      (forget-value! result me)
      (forget-value! m1 me)
      (forget-value! m2 me)
      (process-new-value))
    (define (me request)
      (cond ((eq? request 'I-have-a-value)
             (process-new-value))
            ((eq? request 'I-lost-my-value)
             (process-forget-value))
            (else
             (error "Unknown request - DIVIDER" request))))
    (connect m1 me)
    (connect m2 me)
    (connect result me)
    me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request - CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)


(define (make-connector)
    (let ((value false) (informant false) (constraints '()))
      (define (set-my-value newval setter)
        (cond ((not (has-value? me))
               (set! value newval)
               (set! informant setter)
               (for-each-except setter
                                inform-about-value
                                constraints))
              ((not (= value newval))
               (error "Contradiction" (list value newval)))
              (else 'ignored)))
      (define (forget-my-value retractor)
        (if (eq? retractor informant)
            (begin (set! informant false)
                   (for-each-except retractor
                                    inform-about-no-value
                                    constraints))
            'ignored))
      (define (connect new-constraint)
        (if (not (memq new-constraint constraints))
            (set! constraints
                  (cons new-constraint constraints)))
        (if (has-value? me)
            (inform-about-value new-constraint))
        'done)
      (define (me request)
        (cond ((eq? request 'has-value?)
               (if (not (eq? informant false)) true false)) ; Had to change this from the book
              ((eq? request 'value) value)
              ((eq? request 'set-value!) set-my-value)
              ((eq? request 'forget) forget-my-value)
              ((eq? request 'connect) connect)
              (else (error "Unknown operation - CONNECTOR"
                           request))))
      me))

(define (gravity f m1 m2 r)
    (let ((mult     (make-connector))
          (r-square (make-connector))
          (divided  (make-connector))
          (g        (make-connector)))

      (multiplier m1 m2 mult)
      (multiplier r r r-square)
      (divider mult r-square divided)
      (multiplier g divided f)
      (constant 0.00667300 g)
      'ok))

(define (run4)
    (define f (make-connector))
    (define m1 (make-connector))
    (define m2 (make-connector))
    (define r (make-connector))
    (gravity f m1 m2 r)
    (set-value! m1 25 this)
    (set-value! m2 4 this)
    (set-value! r 4 this)
    (inspect (get-value f)))


;===================================Task 5======================================

(define (barrier)
    (define (set threadNum)
        (if (= threadNum 0)
            nil
            (begin
                (tjoin (thread (gettid)))
                (set (- threadNum 1)))))
    (define (install) (lock))
    (define (remove) (unlock))
    this)

(define (run5)
    (debugSemaphore #t)
    (define b (barrier))
    ((b 'set) 3)
    ((b 'install))
    ((b 'remove)))


;===================================Task 6======================================

(define scar stream-car)
(define scdr stream-cdr)
(define scons cons-stream)

(define (svdisplay s n)
	(cond
		((= n 1) (print (scar s)))
		((> n 1) (print (scar s) ",") (svdisplay (scdr s) (- n 1)))
		(else nil)))
(define (stream-display s n) (print "[") (svdisplay s n) (println "...]"))
(define (stream-ref s n)
    (if (= n 0)
        (scar s)
        (stream-ref (scdr s) (- n 1))))

(define (divisible? x y) (= (remainder x y) 0))

(define (integers-starting-from n)
    (if (or (seven-primes n) (eleven-primes n))
        (scons n (integers-starting-from (+ n 1)))
        (integers-starting-from (+ n 1))))

(define (big-gulp)
    (integers-starting-from 7))

(define (seven-primes n)
    (if (or (= n 7) (= n 11))
        #t
        (and (divisible? n 7) (seven-primes (/ n 7)))))

(define (eleven-primes n)
    (if (or (= n 11) (= n 7))
        #t
        (and (divisible? n 11) (eleven-primes (/ n 11)))))

(define bgs (big-gulp))

(define (run6)
    (stream-display bgs 9))


;===================================Task 7======================================

(define (stream-map proc @)
    (if (stream-null? (car @))
        the-empty-stream
        (scons
            (apply proc (map scar @))
            (apply stream-map
                (cons proc (map scdr @))))))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define (sub-streams s1 s2)
    (stream-map - s1 s2))

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x  factor)) stream))

(define (scale-back-stream stream factor)
    (stream-map (lambda (x) (/ (real x) (real factor))) stream))

(define (signal f x dx)
    (scons (f x) (signal f (+ x dx) dx)))

(define (sop op s t)
    (scons (op (scar s) (scar t))
           (sop op (scdr s) (scdr t))))

(define (integral s dx)
    (define int
        (scons (* (scar s) dx)
               (add-streams (scale-stream (scdr s) dx)
                            int)))
    int)

(define (differential start s dx)
    (define int
        (scons start
            (scons (- start (scar s))
                   (sub-streams int
                                (scdr s)))))
    int)

(define poly (signal (lambda (x) (- (+ (* x x) (* 3 x)) 4)) 0 0.000001))

(define intPoly (integral poly 0.000001))

(define divIntPoly (differential (scar poly) intPoly 0.000001))

(define substreams (stream-map - poly divIntPoly))

(define (run7)
    (stream-display poly 5)
    (stream-display intPoly 5)
    (stream-display divIntPoly 5)
    (stream-display substreams 5))

(run7)


;===================================Task 8======================================

(define (fact n)
    (if (or (= n 1) (= n 0))
        1
        (* n (fact (- n 1)))))

; ((x ^ e) / e!)
(define (mystery x)
    (define (mystery-stream term e)
        (if (even? term)
            (scons (/ (real (^ x e)) (real (fact e))) (mystery-stream (+ term 1) (+ e 2)))
            (scons (- (/ (real (^ x e)) (real (fact e)))) (mystery-stream (+ term 1) (+ e 2)))))
    (mystery-stream 0 0))

(define (ps-mystery x)
    (define sum
        (scons
            (scar (mystery x))
            (add-streams sum (scdr (mystery x)))))
    sum)

(define (euler-transform s)
    (define (square x)
        (^ x 2))
    (let ((s0 (stream-ref s 0))
          (s1 (stream-ref s 1))
          (s2 (stream-ref s 2)))
        (scons (- s2 (/ (real (square (- s2 s1)))
                        (real (+ s0 (* (- 2) s1) s2))))
               (euler-transform (scdr s)))))

(define (acc-mystery x)
    (euler-transform (ps-mystery x)))

(define (make-tableau t s)
    (scons s (make-tableau t (t s))))

(define (super-mystery x)
    (stream-map scar (make-tableau euler-transform (ps-mystery x))))

(define (run8)
    (stream-display (mystery 1) 5)
    (stream-display (ps-mystery 1) 5)
    (stream-display (acc-mystery 1) 5)
    (stream-display (super-mystery 1) 5))


;===================================Task 9======================================

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1carweight (weight (stream-car s1)))
                (s2carweight (weight (stream-car s2))))
            (cond ((> s1carweight s2carweight)
                   (cons-stream (stream-car s2)
                                (merge-weighted s1 (stream-cdr s2) weight)))
                  ((< s1carweight s2carweight)
                   (cons-stream (stream-car s1)
                                (merge-weighted (stream-cdr s1) s2 weight)))
                  (else
                    (cons-stream (stream-car s1)
                                 (cons-stream (stream-car s2)
                                              (merge-weighted (stream-cdr s1) (stream-cdr s2) weight)))))))))

(define (weighted-pairs s t weight)
  (cons-stream (list (stream-car s) (stream-car t))
               (merge-weighted
                 (stream-map (lambda (x) (list (stream-car s) x))
                             (stream-cdr t))
                 (weighted-pairs (stream-cdr s)
                                 (stream-cdr t)
                                 weight)
                 weight)))

(define (cube-sum pair)
    (let ((i (car pair))
          (j (cadr pair)))
      (+ (* i i i) (* j j j))))

;(define sortedStream (weighted-pairs cube-sum (integers-starting-from 0)
                                            ;  (integers-starting-from 0)))

(define (ramanujan s pre)
    (let ((num (cubicSum (stream-car s))))
        (cond ((= pre num)
                   (cons-stream num (ramanujan (stream-cdr s) num)))
              (else (ramanujan (stream-cdr s) num)))))



;(define (run9)
;    ;(display-stream (ramanujan sortedStream 0))
;    (stream-display (ramanujan) 5))

(println "assignment 3 loaded!")
