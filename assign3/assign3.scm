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
    (inspect (test1 1 2 3)))


;===================================Task 3======================================

;===================================Task 4======================================

;===================================Task 5======================================

;===================================Task 6======================================

;Notes 11/10/16

(define scar stream-car)
(define scdr stream-cdr)
(define scons cons-stream)

(define (svdisplay s n)
	(cond
		((= n 1) (print (scar s)))
		((> n 1) (print (scar s) ",") (svdisplay (scdr s) (- n 1)))
		(else nil)))

(define (stream-display s n) (print "[") (svdisplay s n) (println "...]"))
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
    (stream-display bgs 5))

(run6)

;===================================Task 7======================================

;===================================Task 8======================================

;===================================Task 9======================================

(println "assignment 3 loaded!")
