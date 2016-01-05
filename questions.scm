(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))

; Some utility functions that you may find useful to implement.
(define (map proc items)
    (if (null? items) nil

    (cons (proc (car items)) (map proc (cdr items)))

    )


  )
(define (cons-all first rests)
    (cond
    ((null? rests) (cons first nil))
    ((null? (cdr rests)) (cons (cons first (car rests)) nil))

    (else (cons (cons first (car rests)) (cons-all first (cdr rests))))

    )


)


(define (zip pairs)
    (define first (lambda (x) (car x)))
    (define second (lambda (x) (cadr x)))
   (list (map first pairs) (map second pairs))

 )

;; Problem 18
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN Question 18
    (define (helper s n)
        (if (null? s) nil
        (cons (cons n (cons (car s) nil)) (helper (cdr s) (+ n 1)))
        )

    )
    (helper s 0)

  )
  ; END Question 18




(define (list-change total denoms)
 ; BEGIN Question 19

    (cond
        ((null? denoms) nil)
        ((<= total 0) (cons nil nil) )
        ((< total 0) nil)
        ((< total (car denoms)) (list-change total (cdr denoms)))
         (else

          (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms))

           (list-change total (cdr denoms)))
          )
    )

)
  ; END Question 19




;; Problem 20
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (analyze expr)
  (cond ((atom? expr)
         ; BEGIN Question 20
            expr
         ; END Question 20
         )
        ((quoted? expr)
         ; BEGIN Question 20
            expr
        ; END Question 20
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
              (cons form (cons params (analyze body)))

           ; END Question 20
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
                (define z (analyze (zip values)))
                (cons (cons 'lambda (cons (analyze (car z)) (cons (analyze (car body)) nil))) (analyze (cadr  z)))

           ; END Question 20
           ))
        (else
         ; BEGIN Question 20
            (define (other expr)
             (if (null? expr) nil
              (cons (analyze (car expr)) (other (cdr expr)))
                )
            )
            (other expr)
          ; END Question 20
         )))




;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  ; BEGIN Question 21
  'REPLACE-THIS-LINE
  )
  ; END Question 21

