(display "HELLO")
(newline)

(define foo (lambda (x) x))

;PART A

(define foldr (lambda (f i L)
                (if (null? L)
                    i
                    (f (car L) (foldr f i (cdr L)))
                    ))
  )

(define foldl (lambda (f i L)
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
(define L (list 1 #f 2 5 0))
(define p boolean?)
(foldr
 (lambda (a b)
   (if (not b) (p a) b))
 #f L)

(newline)
(display "5.")
(define L (list 1 2 3 4))
(define f foo)
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
; Therefore, strictly speaking, foldl is a better choice than tail-recursive foldr but it is an insignificant difference.

;2.
; This implementation is not tail-recursive so it uses O(n) memory. The function still just traverses the list once so the runtime is the same (which is O(n)).