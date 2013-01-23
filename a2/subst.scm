;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subst.scm
;; Written for CS 442/642
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some functions for manipulating lambda expressions.
;;
;; (make-abs <var> <expr>) creates the encoding for an abstraction
;; (var-of <abs>) return the variable of an abstraction
;; (body-of <abs>) return the body of an abstraction
;;
;; (make-app <rator> <rand>) creates the encoding for an application
;; (rator-of <app>) return the function of an application
;; (rand-of <app>) return the argument of an application
;;
;; (abs? <expr>) predicate to identify abstractions
;; (app? <expr>) predicate to identify applications
;; (var? <expr>) predicate to identify variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-abs (lambda (var body) (list 'fun var body)))
(define make-app (lambda (rator rand) (list rator rand)))

(define abs? (lambda (expr) 
  (and (list? expr) (= (length expr) 3) (eqv? 'fun (car expr)))))
(define app? (lambda (expr)
  (and (list? expr) (= (length expr) 2))))
(define var? symbol?)

(define var-of cadr)
(define body-of caddr)

(define rator-of car)
(define rand-of cadr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Substitution 
;; - Implements Dynamic Binding
;;
;; [e/x]y = y  (if x != y)
;; [e/x]x = e
;; [e/x](e1 e2) = ([e/x]e1 [e/x]e2)
;; [e/x](fun y e1) = (fun y [e/x]e1)  (if x != y)
;; [e/x](fun x e1) = (fun x e1)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define substd (lambda (e x expr)   
    (cond 
      ((abs? expr)
         (if (eqv? x (var-of expr))
             expr        ;; [e/x](fun x e1) = (fun x e1)
             (make-abs   ;; [e/x](fun y e1) = (fun y [e/x]e1)
                (var-of expr) (substd e x (body-of expr))
             )
         )
      )
      ((app? expr)      ;; [e/x](e1 e2) = ([e/x]e1 [e/x]e2)
         (make-app (substd e x (rator-of expr))
                   (substd e x (rand-of expr))
         )
      )
      ((var? expr)
         (if (eqv? x expr)
             e       ;; [e/x]x = e
             expr    ;; [e/x]y = y
         )
      )
      (else expr)    ;; Error! Just return the expr
    )
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Substitution 
;; - Implements Static Binding
;;
;; [e/x]y = y  (if x != y)
;; [e/x]x = e
;; [e/x](e1 e2) = ([e/x]e1 [e/x]e2)
;; [e/x](fun y e1) = (fun y [e/x]e1)  (if x != y && y is not FV[e])
;; [e/x](fun y e1) = (fun z [e/x][z/y]e1)  (if x != y && y is FV[e])
;; [e/x](fun x e1) = (fun x e1)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check if var is a free variable in expr
(define free-in?
  (lambda (var expr)
    (cond
      ((abs? expr)
         (if (eqv? var (var-of expr))
             #f
             (free-in? var (body-of expr))
         )
      )
      ((app? expr)
         (or (free-in? var (rator-of expr))
             (free-in? var (rand-of expr))
         )
      )
      ((var? expr)
         (if (eqv? var expr)
             #t
             #f
         )
      )
      (else #f)    ;; Error! Just return true...
    )
    )
  )

;; Return a "new" var
;; Currently just returns 'NEW assuming expr does not contain this var
(define new-var-for
  (lambda (expr)
    'new
    )
  )

(define subst (lambda (e x expr)
    (cond 
      ((abs? expr)
         (if (eqv? x (var-of expr))
             expr        ;; [e/x](fun x e1) = (fun x e1)
             (if (free-in? (var-of expr) e)
                 (make-abs   ;; [e/x](fun y e1) = (fun z [e/x][z/y]e1)
                     (new-var-for (body-of expr)) (subst e x (subst (new-var-for (body-of expr)) (var-of expr) (body-of expr)))
                 )
                 (make-abs   ;; [e/x](fun y e1) = (fun y [e/x]e1)
                     (var-of expr) (subst e x (body-of expr))
                 )
             )
         )
      )
      ((app? expr)      ;; [e/x](e1 e2) = ([e/x]e1 [e/x]e2)
         (make-app (subst e x (rator-of expr))
                   (subst e x (rand-of expr))
         )
      )
      ((var? expr)
         (if (eqv? x expr)
             e       ;; [e/x]x = e
             expr    ;; [e/x]y = y
         )
      )
      (else expr)    ;; Error! Just return the expr
    )
))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A simple test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(define e1 (make-abs 'x (make-abs 'y (make-app 'z 'y))))

(display (subst 'a 'z e1))

(newline)
(newline)
"TEST 1:"

(newline)
"dynamic"
(display (substd (make-app 'y 'y) 'z e1))

(newline)
"static"
(display (subst (make-app 'y 'y) 'z e1))

(newline)
(newline)
"TEST 2:"

(newline)
"dynamic"
(display (substd (make-abs 'y 'y) 'z e1))

(newline)
"static"
(display (subst (make-abs 'y 'y) 'z e1))
 
