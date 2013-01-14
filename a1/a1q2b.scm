;****************************************
; CS442 Assignment 1 - Question 2
; h55li 20300403
;****************************************

; a1q2b.scm
(define counter 0)
(define countme
  (lambda ()
    (set! counter (+ counter 1)) counter
    )
  )