;PART A

(define foldr 
  (lambda (f i L)
    (if (null? L)
        i
        (f (car L) (foldr f i (cdr L)))
        ))
  )

(define foldl 
  (lambda (f i L)
    (if (null? L)
        i
        (foldl f (f (car L) i) (cdr L))
        ))
  )

(define L (list 1 2 3 4 5 4 3 2 1 0))

;PART B

(newline)
(display "1.")
(foldr + 0 L)

(newline)
(display "2.")
(foldr
 (lambda (unused i)
   (+ i 1))
 0 L)

(newline)
(display "3.")
(foldr
 (lambda (new max)
   (if (> new max) new max))
 0 L)

(newline)
(display "4.")
(define L (list 1 2 5 0))
(define p boolean?)
(foldr
 (lambda (a b)
   (if (not b) (p a) b))
 #f L)

(newline)
(display "5.")
(define L (list 1 2 3 4 5 6 0 0 0))
(define f (lambda (x) (+ x 1)))
(foldr
 (lambda (arg tail)
   (cons (f arg) tail))
 '() L)

(newline)
(display "6.")
(define L (list 0 0 0))
(define M (list 1 2 3))
(foldr cons M L)

;PART C

;1.
; A simple implementation of foldr may not use tail-recursion. So foldl would use less (O(1)) memory to run.
; However we can implement one in terms of the other by first reversing L. This means that an O(n) operation can make these two functions equivalent.
; Therefore, there's no (significant) difference between the two.

;2.
; This implementation is more efficient because it stops evaluating when one of the elements of the list evaluates to true.
; So this implementation's average runtime is n/2 while the fold implementation has an average runtime of n.

;****************************************
; CS442 Assignment 1 - Question 2
; h55li 20300403
;****************************************

; a1q2a.scm
(newline)
(display "A ")
(define kill3
  (lambda (L)
    (set! L
          (set-cdr! L
                    (cons (car (cdr L))
                          (cdr (cdr (cdr L))))
                    )
          ))
  )

(define a '(1 2 3 4 5 6 7 8))
(kill3 a)
a

; a1q2b.scm
(newline)
(display "B ")
(define counter 0)
(define countme
  (lambda ()
    (set! counter (+ counter 1)) counter
    )
  )

; a1q2c.scm
(newline)
(display "C ")
(define L '(0 0 0 0))
(map
 (let ((x 0))
   (lambda (y) (set! x (+ x 1)) (+ y x))
   )
 L
 )
