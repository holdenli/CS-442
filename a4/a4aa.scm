; a4 part a a

(define s-car car)
(define s-cdr (lambda (s) (force (cdr s))))

(define lazymap (lambda (f s)
                  (cons (f (s-car s))
                        (delay (lazymap f (s-cdr s)))
                        )
                  )
  )
