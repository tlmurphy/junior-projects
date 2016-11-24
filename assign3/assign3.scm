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

;===================================Task 4======================================

;===================================Task 5======================================

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

(define (big-gulp)
    (define (integers-starting-from n)
        (if (or (seven-primes n) (eleven-primes n))
            (scons n (integers-starting-from (+ n 1)))
            (integers-starting-from (+ n 1))))
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

(println "assignment 3 loaded!")
