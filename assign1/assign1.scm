(define (author)
    (println "AUTHOR: Trevor Murphy tmurphy2@crimson.ua.edu"))

(define (exprTest # $expr target)
    (define result (catch (eval $expr #)))
    (if (error? result)
        (println $expr " is EXCEPTION: " (result'value)
            " (it should be " target ")")
        (println $expr " is " result
            " (it should be " target ")")))

; Task 1
(define (run1)
    ; (define x 10)
    ; (define a 15)
    ; (and (< a x) (= (/ a 0) 0)))    --> No divide-by-zero error :)
    ; (my-and (< a x) (= (/ a 0) 0))) --> divide-by-zero error :(
    (println
        "SHORT CIRCUITING:
        The built-in 'and' operator will automatically resolve
        to false when the first expression given to it is false. This
        protects against the divide-by-zero error that the second expression
        would return.
        my-and does not utilize short circuiting, so the first expression
        returns false, then the second expression is evaluated which results
        in a divide-by-zero error."))

; Task 2
(define (min5 a b c d e)
    (define (m f g) (if (< f g) f g))
    (m a (m b (m c (m d e)))))
(define (run2)
    (inspect (min5 -5 22 3 -44 5)))

; Task 3
(define (cym value)
    (define pi 3.1415926535897932384626)
    ; changing to 2 decimal places is a work around for the 255 to 254 problem
    (define (find_cyan)
        (int (fmt "%.2f" (* 255 (sin(+ (* (/ value 100) (- pi (/ pi 2))) (/ pi 2)))))))
    (define (find_yellow)
        (int (fmt "%.2f" (* 255 (+ 1 (sin(+ (* (/ value 100) (- (* 2 pi) pi)) pi)))))))
    (define (find_magenta)
        (int (fmt "%.2f" (* 255 (/ (+ 1 (sin(+ (* (/ value 100) (- (* 2 pi) (/ pi 2))) (/ pi 2)))) 2)))))
    (string+ "#" (fmt "%02X" (find_cyan)) (fmt "%02X" (find_yellow)) (fmt "%02X" (find_magenta))))
(define (run3)
    (inspect (cym (real 100))))

; Task 4
(define (goodGuess? guess x)
    (define (fifth n) (* n n n n n))
    (< (abs (- (fifth guess) x)) 0.001))

(define (improve guess x)
    (define (fourth n) (* n n n n))
    (/ (+ (* 4 guess) (/ x (fourth guess))) 5))

(define (rootIter guess x)
    (if (goodGuess? guess x)
        guess
        (rootIter (improve guess x) x)))

(define (root5 n)
    (rootIter 1.0 n))

(define (run4)
    (inspect (root5 969696)))

; Task 5
(define (bico i j)
    (cond
        ((< i j) "Sorry this isn't possible!")
        ((or (= j 0) (= i j)) 1)
        (else (+ (bico (- i 1) (- j 1))
              (bico (- i 1) j)))))

(define (run5)
    (inspect (bico 4 1))
    (inspect (bico 4 3)))

; Task 6
(define (curry f x)
    (lambda (a)
        (lambda (b)
            (lambda (c)
                (f x a b c )))))

(define (run6)
    (define (f a b c d) (+ a b c d))
    (inspect (== (f 1 2 3 4) ((((curry f 1) 2) 3) 4))))

; Task 7
(define (zorp i f)
    ; Adapted from the FIB implementation in SICP
    (define (iter a b c counter)
        (if (< counter 3)
            a
            (iter (+ a
                     (/ (^ (- a b) 2)
                        (+ (- c (* 2 b)) a)))
                  a
                  b
                  (- counter 1))))
    (if (< i 3)
        (f i)
        (iter (f 2) (f 1) (f 0) i)))

(define (zorp_recurse i f)
    ; Zorp(i-1) == a
    ; Zorp(i-2) == b
    ; Zorp(i-3) == c
    (if (< i 3)
        (f i)
        (+ (zorp (- i 1) f)
           (/ (^ (- (zorp (- i 1) f)
                    (zorp (- i 2) f)) 2)
              (+ (- (zorp (- i 3) f)
                     (* 2 (zorp (- i 2) f)))
                  (zorp (- i 1) f))))))

(define (run7)
    (define (func n)
        (+ (^ n 3) (^ n 2) n))
    (define (iter i f)
        (inspect (zorp i f))
        (if (< i 20)
            (iter (+ i 1) f)))

    (iter (- 5) func))

; Task 8
(define (double n)
    (+ n n))

(define (halve n)
    (define (checkDouble n original)
        (if (or (= (- original (double n)) 1) (= original (double n)))
            n
            (checkDouble (- n 1) original)))
    (checkDouble n n))

(define (egypt/ dividend divisor)
    (define (iter1 a b)
        (if (> b dividend)
            (iter2 a b dividend 0)
            (iter1 (double a) (double b))))
    (define (iter2 a b c d)
        (if (< a 1)
            d
            (if (<= b c)
                (iter2 (halve a) (halve b) (- c b) (+ a d))
                (iter2 (halve a) (halve b) c d))))
    (iter1 1 divisor))


(define (run8)
    (inspect (egypt/ 100 25)))

; Task 9
(define (mystery numOfTerms augend func1 func2)
    (define (iter depth value)
        (if (= depth 0)
            (+ augend value)
            (iter (- depth 1) (/ (func1 depth) (+ (func2 depth) value)))))
    (iter (real numOfTerms) 0))

(define (denominator i)
    (if (!= (% (int i) 3) 1)
        1
        (+ i (/ (int i) 3))))


(define (run9)
    (inspect (mystery 1000 1 (lambda (n) 1) denominator)))
    (println "This is the square root of e...")

; Task 10
(define (ramanujan depth)
    (define (ramHelper currentDepth i)
        (if (= depth currentDepth)
        (sqrt (+ 6 depth))
        (sqrt (+ (+ 6 currentDepth)
                 (* i (ramHelper (+ currentDepth 1) (+ 1 i)))))))
    (ramHelper 0 2))

(define (iramanujan depth)
    (define (ramHelper x a1 a2)
        (if (< a1 6)
            x
            (ramHelper (sqrt (+ a1 (* a2 x))) (- a1 1) (- a2 1))))
    (ramHelper (sqrt (+ 6 depth)) (+ 5 depth) (+ 1 depth)))

(define (run10)
    (inspect (ramanujan 50))
    (inspect (iramanujan 50)))

(println "assignment 1 loaded!")
