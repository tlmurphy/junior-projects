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
(define (iterate # $i $lyst $)
    (define lyst (eval $lyst #))
    (define $i (eval (car lyst) #))
    (println "the first argument is " $i)
    (println "the second argument is " $lyst)
    (println "the remaining arguments are:")
    (while (valid? $)
        (eval (car $) #)
        (set! $ (cdr $))
        )
    )

    ; (eval lambda #) ??? 'evaluate it in the calling environment'
    ;(define lyst (eval $lyst #))
    ;(cond
    ;    ((null? lyst))
    ;    (else
    ;        ((lambda (lyst)
    ;            (define i (eval (car lyst) #))
    ;            (define inspect1 (eval $inspect1 #))
    ;            (define inspect2 (eval $inspect2 #))) lyst)
    ;        (iterate $i (cdr lyst) $inspect1 $inspect2))))

(define (testIter lyst)
    (cond
        ((null? lyst))
        (else (inspect (car lyst)) (testIter (cdr lyst)))))

(define (run1)
    (iterate i (list 1 2 3 4) (inspect i) (inspect (* i i))))

(run1)
