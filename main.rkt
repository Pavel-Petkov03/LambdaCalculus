#lang racket


(define ctrue
  (lambda (x)
    (lambda (y)
      x
      )
    )
  )

(define cfalse
  (lambda (x)
    (lambda (y)
      y
      )
    )
  )
(define cor
  (lambda (cpred1)
    (lambda (cpred2)
      ((ctrue cpred1) cpred2)
    )
  ))
(define cand
  (lambda (cpred1)
    (lambda (cpred2)
      ( (cpred1 cpred2) cfalse)
      )))
(define cnot
  (lambda (cpred)
    ( (cpred cfalse) ctrue)
    ))
(define cif
  (lambda (cpred)
    (lambda (then)
      (lambda (else)
        ((cpred then) else)
      ))))

(define csucc
  (lambda (cnum)
    (lambda (f)
      (lambda (x)
        (f ((cnum f) x))))))



(define cpair
  (lambda (x)
    (lambda (y)
      (lambda (f)
        ((f x) y)
        )
      )
  ))




(define cfst
  (lambda (p)
    (p ctrue)
  ))

(define csnd
  (lambda (p)
    (p cfalse)
  ))


(define (church->number cnum)
  ((cnum (lambda (x) (+ 1 x))) 0)
  ) 

(define (repeated F n X)
  (if (= n 0) X
      (F (repeated F (- n 1) X))))
 
(define (number->church num)
  (lambda (f)
    (lambda (x)
       (repeated f num x))))
    

(define church->bool
  (lambda (cbool)
    ((cbool #t) #f)
    ))

(define (bool->church pred?)
  (if pred?
      ctrue
      cfalse)
  )
(define cadd
  (lambda (cnum1)
    (lambda (cnum2)
      (lambda (f)
        (lambda (x)
          ((cnum2 f) ((cnum1 f) x))
          )))))

(define cmult
  (lambda (cnum1)
    (lambda (cnum2)
      (lambda (f)
           (cnum2 (cnum1 f))
        ))))

(define cexp
  (lambda (cnum1)
    (lambda (cnum2)
      (cnum2 cnum1)
      )
    ))

(define czero (lambda (f) (lambda (x) x)))
(define cone (lambda (f) (lambda (x) (f x))))

(define cpred
  (lambda (cnum)
     (cfst ((cnum (lambda (p) ; (0,0) -> (0,1) -> (b+1, b+2) -> ... (cnum-1 cnum)
             ( (cpair (csnd p)) (csucc (csnd p)))
           ))
     ((cpair czero) czero)))
  ))

(define cminus
  (lambda (cnum1)
    (lambda (cnum2)
      ((cnum2 (lambda (inner-cnum) (cpred inner-cnum))) cnum1)
    )))

(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (y) ((x x) y))))
     (lambda (x) (f (lambda (y) ((x x) y))))))) ;; y-combinator with delay because of scheme


(define cfact
  (Y (lambda (fact)
    (lambda (cnum)
      ( (cnum (lambda (y) ((cmult cnum) (fact (cpred cnum))))) cone))))
  )


(define is-czero
  (lambda (cnum)
    ((cnum (lambda (y) cfalse)) ctrue)
    ))

(define csub
  (lambda (cnum1)
    (lambda (cnum2)
      ((cnum2 cpred) cnum1)
      )
    ))

(define c=
  (lambda (cnum1)
    (lambda (cnum2)
      ((cand (is-czero ((csub cnum1) cnum2))) (is-czero ((csub cnum2) cnum1)))
    )
  ))

(define c<
  (lambda (cnum1)
    (lambda (cnum2)
      (cnot (is-czero ( (csub cnum2) cnum1)))
      )
    ))

(define cquot
  (lambda (n)
    (lambda (m)
      (csnd ( (n (lambda (p)
                    ( ( ( (c< (cfst p)) m)
                       p
                       ) ( (cpair ( (cminus (cfst p)) m)) (csucc (csnd p)))
                         )))
                 ( (cpair n) czero)
                 )))
      ))
        
;; moga da naprqvq copy -> filter(lambda x : True, ls)

(define crem
  (lambda (n)
    (lambda (m)
       ((csub  n ) ( (cmult ( (cquot n) m)) m) )
      )))

(define crem2
  (lambda (n)
    (lambda (m)
      ( (n
       (lambda (cnum)
         ( ( ( (c< cnum) m) cnum ) ( (csub cnum) m )
         )
      )) n))))



(define ctwo (number->church 2))

(define cprime
  (lambda (n)
    (csnd ( ( (cpred (cpred n) ) (lambda (p)
           ( ( (is-czero ( (crem2 n) (cfst p)))
             ( (cpair (cfst p)) cfalse)
         
         )
             ( (cpair (csucc (cfst p)) ) ctrue)
             ))) 
      ((cpair ctwo) ctrue)
    ))))

(define cexp2
  (lambda (cnum1)
    (lambda (cnum2)
      ( (cnum2 (lambda (f) ( (cmult cnum1) f) )) cone)
      )))

(define chyp
  (lambda (cnum1)
    (lambda (cnum2)
      ( (cnum2 (lambda (f) ( (cexp2 cnum1) f) )) cone)
      )))

(provide (all-defined-out))
