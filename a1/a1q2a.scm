;****************************************
; CS442 Assignment 1 - Question 2
; h55li 20300403
;****************************************

; a1q2a.scm
(define kill3
  (lambda (L)
    (set! L
          (set-cdr! L
                    (cons (car (cdr L))
                          (cdr (cdr (cdr L))))
                    )
          ))
  )