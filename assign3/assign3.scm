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
    (define body (cdr (get 'code func)))

    (define (nesting body nestedLocals)
        (cond
            ((null? body) nestedLocals)
            ((pair? (car body)) (nesting (cdr body) (append (nesting (car body) nestedLocals) nestedLocals)))
            (else
                (if (equal? (type (car body)) 'SYMBOL)
                    (nesting (cdr body) (cons (car body) nestedLocals))
                    (nesting (cdr body) nestedLocals)))))

    (define (in? a list)
        (if (null? list)
            #f
            (if (equal? a (car list))
                #t
                (in? a (cdr list)))))

    (define (removeDuplicates list newList)
        (if (null? list)
            newList
            (if (not (in? (car list) newList))
                (removeDuplicates (cdr list) (cons (car list) newList))
                (removeDuplicates (cdr list) newList))))

    (define (getLocals body locals)
        (cond
            ((null? body) locals)
            (else
                (append (nesting (car body) '()) (getLocals (cdr body) locals)))))

    (define (removeLocals list newList)
        (if (null? list) newList
            (if (not (in? (car list) locals))
                (removeLocals (cdr list) (cons (car list) newList))
                (removeLocals (cdr list) newList))))

    (cons 'begin (removeDuplicates (removeLocals (getLocals body '()) '()) '())))


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
    ;(inspect (nonlocals square))
    (inspect (nonlocals test2)))

(run1)

;(define (iter returnLyst body)
;    (define (getNonLocals item list notLocal)
;        (println (car list))
;        (cond
;            ((null? list) #f)
;            ((not (equal? item (car list))) getNonLocals item (cdr list) (cons (car list) notLocal))
;            (else
;                (getNonLocals item (cdr list) notLocal))))
;    (if (null? body)
;        returnLyst
;        (iter (append returnLyst
;                      (getNonLocals local (car body) returnLyst))
;              (cdr body))))
;
;(iter '() (cdr body)))
;
;(append (iterateLocals locals '()) (list 'begin)))

;===================================Task 2======================================

;===================================Task 3======================================

;===================================Task 4======================================

;===================================Task 5======================================

;===================================Task 6======================================

;===================================Task 7======================================

;===================================Task 8======================================

;===================================Task 9======================================

(println "assignment 3 loaded!")
