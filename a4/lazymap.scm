(define s-car car)
(define s-cdr (lambda (s) (force (cdr s))))

(define ones (cons 1 (delay ones)))

(define lazymap (lambda (f s)
                  (cons (f (s-car s))
                        (delay (lazymap f (s-cdr s)))
                        )
                  )
  )