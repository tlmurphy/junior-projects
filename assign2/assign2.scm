(define (author)
        (println "Trevor Murphy tmurphy2@crimson.ua.edu"))

(define (exprTest # $expr target)
        (define result (catch (eval $expr #)))
        (if (error? result)
            (println $expr " is EXCEPTION: " (result'value)
                " (it should be " target ")")
            (println $expr " is " result
                " (it should be " target ")")))

; Task 1
(define (iterate # $x items $)
    (define l
        (cons 'lambda (cons (cons $x nil) $)))
    (define iterator (eval l #))
    (map iterator items))

(define (run1)
    (iterate i (list 1 2 3 4) (inspect i) (inspect (* i i))))

; Task 2
; Task 3
; Task 4
; Task 5
; Task 6
; Task 7

; Task 8
(define (cxr sym)
    (define (helper x sym)
        (define char (car (string sym)))
        (cond
            ((equal? (string sym) "nil") x)
            ((equal? char "a") (car (helper x (cdr (string sym)))))
            ((equal? char "d") (cdr (helper x (cdr (string sym)))))))
    (lambda (x) (helper x sym)))

(define (run8)
    (inspect ((cxr 'add) '(1 2 3 4 5 6))))

; Task 9
; Task 10

(run8)
