#lang racket
(require "main.rkt")


(define Z
  (lambda (f)
    ((lambda (x) (f (lambda (z) ((x x) z))))
     (lambda (x) (f (lambda (z) ((x x) z)))))))

(define gamma!!
  (lambda (f)
    (lambda (n)
      (((c=0 n) (lambda (y) (c1 y)))
       (lambda (y) (((c* n) (f (cp n))) y))))))

(define c!! (Z gamma!!))

(define primitive-rec
  (lambda (F)
    (lambda (G)
      (Z (lambda (h)
           (lambda (xvector)
             (lambda (y)
               ( ( (c=0 y) (F xvector)) (lambda (z) ( ( ( (G xvector) (cp y)) ( (h xvector) (cp y))) z))  )
               )
             )
           )
      )
    ))) ; обграждам го в ламбда за да не заради начина на оценяване на scheme(правим една ета експанзия при G)

(define fact
  ( (primitive-rec
   (lambda (x) c1)
   ) (lambda (x)
       (lambda (y)
         (lambda (z)
           ( (c* (cs y)) z)
           )
         )
       )))

; (church->number ( (fact (lambda (_) _)) (number->church 4))) 24

; функция на акерман с Z комбинатор
(define A-with-combinator
  (Z
   (lambda (A)
     (lambda (m)
       (lambda (n)
         ( ( (c=0 m) (cs n)) ( ( (c=0 n) (lambda (z) ( ( (A (cp m)) c1) z))) (lambda (z) ( ( (A (cp m)) (lambda (t) ( ( (A m) (cp n)) t))) z) ))
     )
   )
  ))))

(define A-no-combinator
  (lambda (A)
    (lambda (m)
      (lambda (n)
        ( (m (lambda (f) ( (n (lambda (t)  ( ( (A A) (cp m) )( ( (A A) m) (cp n)))   )) ( ( (A A) (cp m)) c1)))) (cs n))
    ))))
   

(define А; 
  (lambda (m)
    (lambda (n)
      ( ( (A-no-combinator A-no-combinator) m ) n)
      )
    )
  )
  


