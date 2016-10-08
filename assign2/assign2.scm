; Assignment 2
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

(define (iterate # $x items $)
    (define l
        (cons 'lambda (cons (cons $x nil) $)))
    (define iterator (eval l #))
    (map iterator items))

(define (run1)
    (iterate i (list 1 2 3 4) (inspect i) (inspect (* i i))))


;===================================Task 2======================================

(define (peval f @)
    (define args @)
    (define returnArgs (list))
    (define (replaceMissing args largs returnArgs)
        (if (equal? (string args) "nil")
            returnArgs
            (if (equal? 'MISSING (car args))
                (replaceMissing (cdr args) (cdr largs) (append returnArgs (list (car largs))))
                (replaceMissing (cdr args) largs (append returnArgs (list (car args)))))))
    (lambda (@) (apply f (replaceMissing args @ returnArgs))))

(define (run2)
    (define . 'MISSING)
    (define (f x y z) (+ x y z))
    (define (g a b c d e f g h i j k)
        (+ (- (+ (* k j) i) h) g f e d c b a))
    (define (h)
        5)
    (define (i a)
        (* a a))
    (exprTest (f 1 2 3) 6)
    (exprTest ((peval f 1 2 3)) 6)
    (exprTest ((peval f . . .) 1 2 3) 6)
    (exprTest ((peval f . 2 .) 1 3) 6)
    (exprTest ((peval f . . 3) 1 2) 6)
    (exprTest ((peval f 1 . 3) 2) 6)
    (exprTest ((peval f 1 2 3)) 6)
    (exprTest ((peval g 1 . 3 4 5 12 13 . 23 . .) 2 14 1 2) 51)
    (exprTest ((peval g 1 2 . 4 5 12 . 14 23 1 2) 3 13) 51)
    (exprTest ((peval g . . . 4 5 12 13 14 23 . .) 1 2 3 1 2) 51)
    (exprTest (h) 5)
    (exprTest ((peval h)) 5)
    (exprTest ((peval i 4)) 16)
    (exprTest ((peval i .) 4) 16))


;===================================Task 3======================================

; Stack Class and Methods
(define (Stack) '())
;(define (person)
;        (define name)
;        (define age)
;        this
;        )
;(define (Stack)
;    (define store '())
;    (define size 0)
;    this)

(define (push stack data)
    (cons data stack))
;(define (push stack data)
;    (cons data (stack 'store)))
(define (pop stack)
    (cdr stack))
(define (speek stack)
    (car stack))
(define (ssize stack)
    (length stack))

; Queue Class and Methods
(define (Queue) '())
(define (enqueue queue data)
    (append queue (list data)))
(define (dequeue queue)
    (cdr queue))
(define (qpeek queue)
    (car queue))
(define (qsize queue)
    (length queue))

(define (run3)
    (define (loop stack queue)
        (define x (readInt))
        (if (eof?)
            (list stack queue)
            (loop (push stack x) (enqueue queue x))))
    (define (popper s)
        (cond
            ((!= (ssize s) 0)
                ;(inspect (speek s))
                (popper (pop s)))))
    (define (dequeuer q)
        (cond
            ((!= (qsize q) 0)
                ;(inspect (qpeek q))
                (dequeuer (dequeue q)))))
    (define oldStream (setPort (open "data.ints" 'read)))
    (define data (loop (Stack) (Queue)))
    (popper (car data))
    (dequeuer (cadr data))
    (setPort oldStream))


;===================================Task 4======================================

(define (no-locals code)
    (define def (cons (car code) (cons (car (cdr code)) nil)))
    (define (iter c params args body)
        (cond
            ((equal? (string c) "nil")
                (append def (cons (cons (cons 'lambda (cons params (cons body nil))) args) nil)))
            (else
                (if (and (> (length c) 1) (equal? (car (car c)) 'define))
                    (iter (cdr c) (append params (list (cadr (car c)))) (append args (list (caddr (car c)))) body)
                    (iter (cdr c) params args (append body (car c)))))))
    (iter (cdr (cdr code)) '() '() '()))

(define (run4)
    (println (no-locals '(define (f) (define x 3) 1)))
    (println (no-locals
        '(define (nsq a)
            (define x (+ a 1))
            (define y (- a 1))
            (if (= x 0)
                (+ x 100)
                (* x y)))))
    (println (no-locals
        (quote
            (define (nsq a)
                (define x (+ a 1))
                (* x x))))))


;===================================Task 5======================================


;===================================Task 6======================================

(define (treeNode value left right)
    (list value left right))

(define (constructTree)
    (treeNode 5
        (treeNode 3 (treeNode 1 nil nil) (treeNode 5 nil nil))
        (treeNode 9 nil (treeNode 11 nil nil))))
(define (treeflatten)
    )
(define (treedepth tree))
    ; return a real number plz)

(define (run6)
    (define tree (constructTree))
    (println tree))

;===================================Task 7======================================


;===================================Task 8======================================

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


;===================================Task 9======================================


;===================================Task 10=====================================

(run6)
