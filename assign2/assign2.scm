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
(define (iterate # $iterable)
    (lambda (i)
        ; bind each value in iterable to i
        )
    ; pre
    (define i (get $i #))
    (define iterable (get $iterable #))

    ; body

    ; post
    (set $i i #)
    (set $iterable iterable #))

(define (run1)
    (iterate i (list 1 2 3 4)
        (inspect i)
        (inspect (* i i))))

(run1)
