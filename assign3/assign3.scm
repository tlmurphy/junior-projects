; Assignment 3
; CS 403 - Programming Languages
; Written by Trevor Murphy

(define (author)
    (println "Trevor Murphy tmurphy2@crimson.ua.edu"))

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
    (define body (get 'body func))
    (define (iterateLocals l returnLyst)
        (cond
            ((null? l) returnLyst)
            (else
                (iterateLocals (cdr l) (append returnLyst (removeLocals (car l))))
                )))
    (define (removeLocals local)
        (define (iter returnLyst body)
            (if (null? body)
                returnLyst
                (if (not (equal? local (caar body)))
                    (append returnLyst (caar body))
                    (iter returnLyst (cdr body)))))
        (iterr '() body))
    (iterateLocals locals ,'(begin )))

(define (run1)
    (define (square x) (* x x) (* x x x))
    (exprTest (nonlocals square) '(begin *)))

(run1)


(println "assignment 3 loaded!")
