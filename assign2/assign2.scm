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

; constructor function is used to create a stack and queue
(define (constructor lyst size) this)

; Stack Class and Methods
(define (Stack) (constructor nil 0))
(define (push stack data)
    (constructor (cons data (stack 'lyst)) (+ (stack 'size) 1)))
(define (pop stack)
    (constructor (cdr (stack 'lyst)) (- (stack 'size) 1)))
(define (speek stack)
    (car (stack 'lyst)))
(define (ssize stack)
    (stack 'size))

; Queue Class and Methods
(define (Queue) (constructor nil 0))
(define (enqueue queue data)
    (constructor (append (queue 'lyst) (list data)) (+ (queue 'size) 1)))
(define (dequeue queue)
    (constructor (cdr (queue 'lyst)) (- (queue 'size) 1)))
(define (qpeek queue)
    (car (queue 'lyst)))
(define (qsize queue)
    (queue 'size))

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
        ;(println "C: " c)
        ;(println "params: " params)
        ;(println "args: " args)
        ;(println "body: " body)
        ;(println "LENGTH: " (length c))
        (cond
            ((null? c)
                (append def (cons (cons (cons 'lambda (cons params (cons body nil))) args) nil)))
            (else
                (if (and (> (length c) 1) (equal? (car (car c)) 'define))
                    (iter (cdr c) (append params (list (cadr (car c)))) (append args (list (caddr (car c)))) body)
                    (iter (cdr c) params args (append body (car c)))))))
    (iter (cdr (cdr code)) '() '() '()))

(define (run4)
    ;(inspect (no-locals '(define (f) (define x 3) 1)))
    ;(inspect (no-locals
    ;    '(define (nsq a)
    ;        (define x (+ a 1))
    ;        (define y (- a 1))
    ;        (if (= x 0)
    ;            (+ x 100)
    ;            (* x y)))))
    ;(inspect (no-locals
    ;    (quote
    ;        (define (nsq a)
    ;            (define x (+ a 1))
    ;            (* x x)))))

    (inspect (no-locals
        '(define (nsq a)
            (define square (lambda (x) (* x x)))))))

    (define (nsq a)
        (define square (lambda (x) (* x x)))
            (square a))

    (define (nsq2 a) ((lambda (square) (square 9)) (lambda (x) (* x x))))


(inspect (nsq 9))
(inspect (nsq2 9))
(run4)


;===================================Task 5======================================

(define pred
    (lambda (n)
        (lambda (f)
            (lambda (x)
                (((n (lambda (a) (lambda (b) (b (a f)))))
                  (lambda (c) x))
                 (lambda (c) c))))))

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
        (println tree)
        (cond
            ((null? tree))
            ((and (equal? (cadr tree) nil) (equal? (caddr tree) nil)) (list (list depth (car tree))))
            (else (append (helper (cadr tree) (+ depth 1))
                          (helper (caddr tree) (+ depth 1))))))
    (helper tree 0))

(define (treedepth tree)
    (define flatTree (treeflatten tree))
    (println flatTree)
    (/ (real (accumulate + 0 (map car flatTree))) (real (length flatTree))))

(define (run6)
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
    (accumulate append '() (map proc seq)))

; Enumerate-Interval function from SICP
(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

(define (adjoin-position row col positions)
    (append positions (cons (cons row (cons col nil)) nil)))

(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

(define (safe? col positions)
    (define (getRow pos) (car pos))
    (define (getCol pos) (car (cdr pos)))

    (define (notSafe q1 q2)
        (if (or (= (getRow q1) (getRow q2)) (= (abs (- (getRow q1) (getRow q2)))
                                               (abs (- (getCol q1) (getCol q2)))))
            #t
            #f))

    (define (iter q board)
        (cond
            ((null? board) #t)
            ((notSafe q (car board)) #f)
            (else (iter q (cdr board)))))

    (iter (list-ref positions col) (filter (lambda (q) (not (= col (getCol q))))
                                           positions)))

(define (sort queens)
    (define row (lambda (q) (car (car q))))
    (define col (lambda (q) (car (cdr (car q)))))

    (define (h2 sortedQueens queens)
        (if (null? queens)
            (reverse sortedQueens)
            (h2 (append sortedQueens (list (list (col queens) (row queens)))) (cdr queens))))

    (define (h sortedQueens queens)
        (if (null? queens)
            (reverse sortedQueens)
            (h (append sortedQueens (list (h2 '() (car queens)))) (cdr queens))))

    (h '() queens))

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
                             (enumerate-interval 0 (- board-size 1))))
                    (queen-cols (- k 1))))))

    (define queenResult (queen-cols (- board-size 1)))

    (if (null? queenResult)
        (list '())
        (sort queenResult)))

(define (run7)
    (inspect (queens 5)))


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
    (putTable 'CONS 'STRING list->string)
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
    (inspect (coerce '(1 (2.2) ((3 4) "5")) 'STRING))
    (inspect (type (coerce '(1 (2.2) ((3 4) "5")) 'STRING)))
    (inspect (coerce '(123.4 (1 (2 1.2)) 678.12) 'STRING)))

(println "assignment 2 loaded!")
