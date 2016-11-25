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

(define (node value)
    (define (helper val l r leftHeight rightHeight height) this)
    (helper value nil nil 0 0 0))

(define (avl)

    (define (helper s root)

        ; Methods
        (define (size) (println s))

        (define (insert n)
            (++ s)
            (define (iter p)
                (if (not (null? p)) (println (p 'val)))
                (if (null? p)
                    (set! root (node n))
                    (if (< (p 'val) n)
                        (if (null? (p 'r))
                            (set! p (node n))
                            (iter (p 'r)))
                        (if (null? (p 'l))
                            (set p (node n) this)
                            (iter (p 'l))))))
            (iter root))

        (define (find n)
            (define (iter root)
                (cond
                    ((null? (this 'root)) #f)
                    ((= ((this 'root) 'val) n) #t)
                    ((< n ((this 'root) 'val)) (iter ((this 'root) 'l)))
                    (else
                        (iter ((this 'root) 'r)))))
            (iter (this 'root)))
        (define (statistics))
        this)
    (helper 0 nil))

(define (run3)
    (define t (avl))
    ((t 'insert) 3)
    (println "ROOT: " ((t 'root) 'val))
    ((t 'insert) 5)
    (println "ROOT: " (((t 'root) 'r) 'val))
    ;((t 'root) 'val)
    ;((t 'insert) 4)
    ;((t 'insert) 5)
    ;((t 'insert) 1)
    ;((t 'insert) 0)
    ;(println ((t 'root) 'val))
    ;(inspect ((t 'find) 5))     ; should return #t
    ;(inspect ((t 'find) 7))     ; should return #f
    ;((t 'size))       ; should return 5
    ((t 'statistics))) ; should print 4:1 1:0 5:0 0:0 3:0

(run3)

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

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream))

(define (signal f x dx)
    (scons (f x) (signal f (+ x dx) dx)))

(define (integral s dx)
    (define int
        (scons (scar s)
               (add-streams (scale-stream s dx)
                            int)))
    int)

(define (differential start s dx)
    (define int
        (scons (scar s)
               (add-streams (scale-stream s dx)
                            int)))
    int)

(define poly (signal (lambda (x) (- (+ (* x x) (* 3 x)) 4)) 0 1))

(define intPoly (integral poly 0.001))

(define divIntPoly (differential (scar intPoly) intPoly 0.001))

(define substreams (stream-map - poly divIntPoly))

(define (run7)
    (stream-display poly 5)
    (stream-display intPoly 5)
    (stream-display divIntPoly 5)
    (stream-display substreams 5))


;===================================Task 8======================================

(define (fact n)
    (if (or (= n 1) (= n 0))
        1
        (* n (fact (- n 1)))))

; ((x ^ e) / e!)
(define (mystery x)
    (define (mystery-stream term e)
        (if (even? term)
            (scons (/ (real (^ x e)) (fact e)) (mystery-stream (+ term 1) (+ e 2)))
            (scons (- (/ (real (^ x e)) (fact e))) (mystery-stream (+ term 1) (+ e 2)))))
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
        (scons (- s2 (/ (square (- s2 s1))
                        (+ s0 (* (- 2) s1) s2)))
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



(define (run9)
    ;(display-stream (ramanujan sortedStream 0))
    (stream-display (ramanujan) 5))

;(run9)
(println "assignment 3 loaded!")
