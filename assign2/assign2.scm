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
    (constructor (cons data (stack 'lyst))
                 (+ (stack 'size) 1)))
(define (pop stack)
    (constructor (cdr (stack 'lyst))
                 (- (stack 'size) 1)))
(define (speek stack)
    (if (= (stack 'size) 0)
        'EMPTY
        (car (stack 'lyst))))
(define (ssize stack)
    (stack 'size))

; Queue Class and Methods
(define (Queue) (constructor nil 0))
(define (enqueue queue data)
    (constructor (append (queue 'lyst) (list data))
                 (+ (queue 'size) 1)))
(define (dequeue queue)
    (constructor (cdr (queue 'lyst))
                 (- (queue 'size) 1)))
(define (qpeek queue)
    (if (= (queue 'size) 0)
        'EMPTY
    (car (queue 'lyst))))
(define (qsize queue)
    (queue 'size))

(define (run3)
    (define newStack (push (push (push (push (push (Stack) 7) 10) 10) 10) 0.33))
    (define newQueue (enqueue (enqueue (enqueue (enqueue (Queue) 10) 50) 50000) 90))
    (exprTest (speek (pop (pop newStack))) 10)
    (exprTest (speek (pop (pop (pop (pop (pop newStack)))))) 'EMPTY)
    (exprTest (ssize (pop (pop (pop newStack)))) 2)
    (exprTest (qpeek newQueue) 10)
    (exprTest (qsize newQueue) 4)
    (exprTest (qsize (dequeue (dequeue (dequeue (dequeue newQueue))))) 0)
    (exprTest (qpeek (dequeue (dequeue (dequeue (dequeue newQueue))))) 'EMPTY))


;===================================Task 4======================================

(define (no-locals code)
    (define def (cons (car code) (cons (car (cdr code)) nil)))
    (define body (cdr (cdr code)))

    (define (iter c params args body)
        (cond
            ((null? c)
                (if (null? body)
                    ; No body but local defines exist
                    (append def (cons (cons (cons 'lambda (cons params nil)) args) nil))
                    ; Body exists
                    (append def (cons (cons (cons 'lambda (cons params (cons body nil))) args) nil))))
            (else
                (define element (car c))
                (if (and (pair? element) (equal? (car element) 'define))
                    (iter (cdr c) (append params (list (cadr element))) (append args (list (caddr element))) body)
                    (iter (cdr c) params args (append body element))))))

    (if (null? body)
        ; Code didn't have a body, so just return the original
        code
        ; Else, perform the conversion
        (iter body '() '() '())))

(define (run4)
    (define func1 '(define (f) (define x 3) 1))
    (define func2 '(define (nsq a)
                       (define x (+ a 1))
                       (define y (- a 1))
                           (if (= x 0)
                               (+ x 100)
                               (* x y))))
    (define func3 '(define (nsq a)
                       (define x (+ a 1))
                       (* x x)))
    (define func4 '(define (nsq a)
                       (define square (lambda (x) (lambda (y) (* x y))))
                       ((square 5) 5)))
    (define func5 '(define (nsq a)))
    (define func6 '(define (nsq) (define square (lambda (x) (* x x)))))
    (define func7 '(define (nsq a)
                       (define square (lambda (x) (lambda (y) (* x y))))
                       ((square 5) 5)))

    (exprTest (= ((eval func1 this)) ((eval (no-locals func1) this))) #t)
    (exprTest (= ((eval func2 this) 3) ((eval (no-locals func2) this) 3)) #t)
    (exprTest (= ((eval func3 this) 69) ((eval (no-locals func3) this) 69)) #t)
    (exprTest (= ((eval func3 this) 69) ((eval (no-locals func3) this) 68)) #f)
    (exprTest (= ((eval func4 this) 69) ((eval (no-locals func4) this) 69)) #t)
    (exprTest (equal? ((eval func5 this) 69) ((eval (no-locals func5) this) 69)) #t)
    ;(exprTest (= (((eval func6 this)) 69) (((eval (no-locals func6) this)) 69)) #t)
    (exprTest (= ((eval func7 this) 69) ((eval (no-locals func7) this) 69)) #t))


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
    (exprTest (equal? (((pred two) f) nil) (((add-1 zero) f) nil)) #t)
    (exprTest (not (equal? ((three inc) 10) (((pred three) inc) 10))) #t)
    (exprTest (equal? (((add-1 (add-1 (add-1 zero))) inc) 45) ((three inc) 45)) #t))


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
    (/ (real (accumulate + 0 (map car flatTree))) (real (length flatTree))))

(define (run6)
    (define (constructTree1)
        (treeNode 7
            (treeNode 3 (treeNode 1 nil nil)
                        (treeNode 5 nil nil))
            (treeNode 9 nil
                        (treeNode 11 (treeNode 4 (treeNode 7 nil nil)
                                                  nil)
                                      (treeNode 40 nil nil)))))
    (define (constructTree2)
        (treeNode 69 nil nil))

    (define (constructTree3)
        (treeNode 9
            (treeNode 9
                (treeNode 1
                    (treeNode 9
                        (treeNode 10 nil nil)
                        (treeNode 90 (treeNode 19
                                         nil
                                         (treeNode 10 nil nil))
                                     (treeNode 20 (treeNode 20
                                                      (treeNode 89 nil nil)
                                                      (treeNode 20 (treeNode 90 nil nil) nil))
                                                  (treeNode 9 nil nil))))
                    (treeNode 20 nil nil))
                (treeNode 69 nil nil))
            (treeNode 6 nil nil)))

    (define tree1 (constructTree1))
    (define tree2 (constructTree2))
    (define tree3 (constructTree3))
    (exprTest (treedepth tree1) 2.7500000000)
    (exprTest (treedepth tree2) 0) ; A bunch of zeros...
    (exprTest (treedepth tree3) 4.6250000000))


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

; List-ref function from SICP
(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

(define (adjoin-position row col positions)
    (append positions (cons (cons row (cons col nil)) nil)))

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
    (define lyst '(1 2 3 4 5 6))
    (exprTest ((cxr 'add) lyst) 3)
    (exprTest ((cxr 'a) lyst) 1)
    (exprTest ((cxr 'ddd) lyst) '(4 5 6))
    (exprTest ((cxr 'ddddd) lyst) '(6))
    (exprTest ((cxr 'addddd) lyst) 6)
    (exprTest (= ((cxr 'adddd) lyst) (caddddr lyst)) #t)
    (exprTest ((cxr 'dddddd) lyst) '()))


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
            ((null? str) "")
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
        ; Get the old operator
        (apply (getTable operator '(BUTTER FINGER))
               (list operand1 operand2))
        ; Get the new operator
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
    'generic-system-installed)

(define (uninstall-generic)
    (set! + old+)
    (set! - old-)
    (set! * old*)
    (set! / old/)
    'generic-system-uninstalled)

(define (run9)
    (install-generic)
    (exprTest (+ 0 0) 0)
    (exprTest (+ "I HATE " "PROGRAMMING") "I HATE PROGRAMMING")
    (exprTest (+ "hello" 4) "hello4")
    (exprTest (+ 68 "1.0") 69)
    (exprTest (+ 89 "a") 89)
    (exprTest (- 0 0) 0)
    (exprTest (- 686868 "686868") 0)
    (exprTest (- 10 "5.6") 5)
    (exprTest (- "abc" 3) "")
    (exprTest (/ 0 90) 0)
    (exprTest (- "abc" 100) "")
    (exprTest (- "qwertyI'm cramping right now" 6) "I'm cramping right now")
    (exprTest (* "ayy" 1) "ayy")
    (exprTest (* "ayy" 0) "")
    (exprTest (* 3 3) 9)
    (exprTest (* "ayy" 3) "ayyayyayy")
    (exprTest (* 3 "23") 69)
    (exprTest (* 0 "0") 0)
    (exprTest (/ 69 "3") 23)
    (exprTest (/ 0 "78888") 0)
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
    (exprTest (coerce "123.4" 'INTEGER) 123)
    (exprTest (coerce 43 'REAL) 43.000000000)
    (exprTest (coerce 899 'STRING) "899")
    (exprTest (coerce 6.333 'INTEGER) 6)
    (exprTest (coerce 32.11412 'STRING) "32.114120")
    (exprTest (coerce "39000" 'INTEGER) 39000)
    (exprTest (coerce "31515.1111" 'INTEGER) 31515)
    (exprTest (coerce "5.33920" 'REAL) 5.3392000000)
    (exprTest (coerce "5" 'REAL) 5.000000000)
    (exprTest (coerce '(1 (2.2) ((3 4) "5")) 'STRING) "(1 (2.200000) ((3 4) 5))")
    (exprTest (type (coerce '(1 (2.2) ((3 4) "5")) 'STRING)) 'STRING)
    (exprTest (coerce '(123.4 (1 (2 1.2)) 678.12) 'STRING) "(123.400000 (1 (2 1.200000)) 678.120000)"))

(println "assignment 2 loaded!")
