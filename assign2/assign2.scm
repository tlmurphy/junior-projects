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

(include "table.scm") ; Used for task 9 and 10


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
        (if (null? args)
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
                (inspect (speek s))
                (popper (pop s)))))
    (define (dequeuer q)
        (cond
            ((!= (qsize q) 0)
                (inspect (qpeek q))
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
            ((null? c)
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
(run4)


;===================================Task 5======================================

(define (pred church)
    (define zero (lambda (f) (lambda (x) x)))
    (define (f a) (cons 1 a))
    (define (add-1 n)
        (lambda (f) (lambda (x) (f ((n f) x)))))
    (define (helper previous current)
        (cond
            ((equal? ((church f) nil) ((current f) nil)) previous)
            (else
                (helper current (add-1 current)))))

    (helper zero (add-1 zero)))

(define (run5)
    (define (inc n)
        (+ n 1))
    (define (f a) (cons 1 a))
    (define zero (lambda (f) (lambda (x) x)))
    (define two (lambda (f) (lambda (x) (f (f x)))))
    (define three (lambda (f) (lambda (x) (f (f (f x))))))
    (define (add-1 n)
        (lambda (f) (lambda (x) (f ((n f) x)))))
    (inspect (equal? (((pred two) f) nil) (((add-1 zero) f) nil))))


;===================================Task 6======================================

(define (treeNode value left right)
    (list value left right))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (treeflatten tree)
    (define (helper tree depth)
        (cond
            ((null? tree))
            ((and (equal? (cadr tree) nil) (equal? (caddr tree) nil)) (list (list depth (car tree))))
            (else (append (helper (cadr tree) (+ depth 1))
                          (helper (caddr tree) (+ depth 1))))))
    (helper tree 0))

(define (treedepth tree)
    (define flatTree (treeflatten tree))
    (accumulate + 0 (map car flatTree))
    (/ (real (accumulate + 0 (map car flatTree))) (real (length flatTree))))

(define (run6)
    (define (treeNode value left right)
        (list value left right))

    (define (constructTree)
        (treeNode 7
            (treeNode 3 (treeNode 1 nil nil)
                        (treeNode 5 nil nil))
            (treeNode 9 nil
                        (treeNode 11 (treeNode 4 (treeNode 7 nil nil)
                                                  nil)
                                      (treeNode 40 nil nil)))))

    (define tree (constructTree))
    (inspect (treedepth tree)))


;===================================Task 7======================================

; Filter function from SICP
(define (filter predicate sequence)
    (cond
        ((null? sequence) nil)
        ((predicate (car sequence)) (cons (car sequence)
                                          (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; FlatMap function from SICP
(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

; Enumerate-Interval function from SICP
(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

(define (getRow position)
    (car position))

(define (getCol position)
    (cdr position))

(define (adjoin-position r k q)
    (cons (cons r (cons k nil)) q))

(define (safe? col positions)
    (println col)
    (println positions))
    ;(define (helper pos)
    ;    (cond
    ;        ((car (getRow positions))))
(safe? 2 '((3 3) (0 3)))

(define empty-board '())

(define (queens board-size)
    (define (queen-cols k)
        (if (< k 0)
            (list empty-board)
            (filter
                (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (rest-of-queens)
                        (map (lambda (new-row)
                                (adjoin-position new-row k rest-of-queens))
                             (enumerate-interval 0 board-size)))
                    (queen-cols (- k 1))))))
    (queen-cols board-size))

;(define (run7)
;    (queens 6))
;
;(run7)

;===================================Task 8======================================

(define (cxr sym)
    (define (helper x sym)
        (define char (car (string sym)))
        (cond
            ((null? sym) x)
            ((equal? char "a") (car (helper x (cdr (string sym)))))
            ((equal? char "d") (cdr (helper x (cdr (string sym)))))))
    (lambda (x) (helper x sym)))

(define (run8)
    (inspect ((cxr 'add) '(1 2 3 4 5 6))))


;===================================Task 9======================================

(define old+ +)
(define old- -)
(define old* *)
(define old/ /)

(define (addStrings a b) (string+ a b))

(define (stringPlusNum a b) (string+ a (string b)))

(define (numPlusString a b) (old+ a (int b)))

(define (numMinusString a b) (old- a (int b)))

(define (stringMinusNum a b)
    (define (helper str num)
        (cond
            ((null? str) "nil")
            ((= num 0) str)
            (else (helper (cdr str) (old- num 1)))))
    (helper a b))

(define (stringTimesNum a b)
    (define (helper str num)
        (if (= num 0)
            str
            (helper (string+ str a) (old- num 1))))
    (helper "" b))

(define (numTimesString a b) (old* a (int b)))

(define (numDivideString a b)
    (old/ a (int b)))

(define (apply-generic operator operand1 operand2)
    (define func (getTable operator (list (type operand1) (type operand2))))
    (if (null? func)
        (apply (getTable operator '(BUTTER FINGER))
               (list operand1 operand2))
        (apply func (list operand1 operand2))))


(define (install-generic)
    (clearTable)
    (set! + (lambda (a b) (apply-generic '+ a b)))
    (set! - (lambda (a b) (apply-generic '- a b)))
    (set! * (lambda (a b) (apply-generic '* a b)))
    (set! / (lambda (a b) (apply-generic '/ a b)))
    (putTable '+ '(STRING STRING) addStrings)
    (putTable '+ '(STRING INTEGER) stringPlusNum)
    (putTable '+ '(INTEGER STRING) numPlusString)
    (putTable '- '(INTEGER STRING) numMinusString)
    (putTable '- '(STRING INTEGER) stringMinusNum)
    (putTable '* '(STRING INTEGER) stringTimesNum)
    (putTable '* '(INTEGER STRING) numTimesString)
    (putTable '/ '(INTEGER STRING) numDivideString)
    ; Old operators
    (putTable '+ '(BUTTER FINGER) old+)
    (putTable '- '(BUTTER FINGER) old-)
    (putTable '* '(BUTTER FINGER) old*)
    (putTable '/ '(BUTTER FINGER) old/)
    'generic-system-installed
    )

(define (uninstall-generic)
    (set! + old+)
    (set! - old-)
    (set! * old*)
    (set! / old/)
    'generic-system-uninstalled
    )

(define (run9)
    (install-generic)
    (inspect (+ "x" "y"))
    (inspect (+ "123" 4))
    (inspect (+ 123 "4"))
    (inspect (- 123 "4"))
    (inspect (- "abc" 1))
    (inspect (* "abc" 3))
    (inspect (* 3 "33"))
    (inspect (/ 8 "2"))
    (inspect (+ 9 0))
    (inspect (/ 10 5))
    (uninstall-generic))

(run9)

;===================================Task 10=====================================

(define (int->real a) (real a))
(define (int->string a) (string a))
(define (real->int a) (int a))
(define (real->string a) (string a))
(define (string->int a) (int a))
(define (string->real a) (real a))
(define (list->string a) (string a))
(define (install-coercion)
    (clearTable)
    (putTable 'INTEGER 'REAL int->real)
    (putTable 'INTEGER 'STRING int->string)
    (putTable 'REAL 'INTEGER real->int)
    (putTable 'REAL 'STRING real->string)
    (putTable 'STRING 'INTEGER string->int)
    (putTable 'STRING 'REAL string->real)
    (putTable 'INTEGER 'STRING int->string)
    'generic-system-installed
    )

(define (coerce v t)
    ((getTable (type v) t) v))

(define (run10)
    (install-coercion)
    (inspect (coerce "123.4" 'INTEGER))
    (inspect (coerce 43 'REAL))
    (inspect (coerce 899 'STRING))
    (inspect (coerce 6.333 'INTEGER))
    (inspect (coerce 32.11412 'STRING))
    (inspect (coerce "39000" 'INTEGER)) (inspect (coerce "31515.1111" 'INTEGER))
    (inspect (coerce "5.33920" 'REAL)) (inspect (coerce "5" 'REAL))
    ;(coerce '(1 (2.2) ((3 4) "5")) 'STRING)
    ;(type (coerce '(1 (2.2) ((3 4) "5")) 'STRING)))
    )

(println "assignment 2 loaded!")
