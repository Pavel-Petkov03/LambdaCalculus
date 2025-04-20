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

(define cs
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
(define c+
  (lambda (cnum1)
    (lambda (cnum2)
      (lambda (f)
        (lambda (x)
          ((cnum2 f) ((cnum1 f) x))
          )))))

(define c* ; c* (Задача 2.20)
  (lambda (cnum1)
    (lambda (cnum2)
      (lambda (f)
           (cnum2 (cnum1 f))
        ))))

(define cexp ; не доказвам тази дефиниция в задачата, защото не работи в дъното (доказвам cexp2)
  (lambda (cnum1)
    (lambda (cnum2)
      (cnum2 cnum1)
      )
    ))

(define czero (lambda (f) (lambda (x) x)))
(define cone (lambda (f) (lambda (x) (f x))))

(define cp
  (lambda (cnum)
     (cfst ((cnum (lambda (p) ; (0,0) -> (0,1) -> (b+1, b+2) -> ... (cnum-1 cnum)
             ( (cpair (csnd p)) (cs (csnd p)))
           ))
     ((cpair czero) czero)))
  ))

(define is-czero
  (lambda (cnum)
    ((cnum (lambda (y) cfalse)) ctrue)
    ))

(define c-
  (lambda (cnum1)
    (lambda (cnum2)
      ((cnum2 cp) cnum1)
      )
    ))

(define c= ; c= (Задача 2.25)
  (lambda (cnum1)
    (lambda (cnum2)
      ((cand (is-czero ((c- cnum1) cnum2))) (is-czero ((c- cnum2) cnum1)))
    )
  ))

(define c< ; c< (Задача 2.25)
  (lambda (cnum1)
    (lambda (cnum2)
      (cnot (is-czero ( (c- cnum2) cnum1)))
      )
    ))

(define cquot ; cquot (Задача 2.26)
  (lambda (n)
    (lambda (m)
      (csnd ( (n (lambda (p)
                    ( ( ( (c< (cfst p)) m)
                       p
                       ) ( (cpair ( (c- (cfst p)) m)) (cs (csnd p)))
                         )))
                 ( (cpair n) czero)
                 )))
      ))
        

(define crem ; crem (Задача 2.26)
  (lambda (n)
    (lambda (m)
       ((c-  n ) ( (c* ( (cquot n) m)) m) )
      )))

(define crem2
  (lambda (n)
    (lambda (m)
      ( (n
       (lambda (cnum)
         ( ( ( (c< cnum) m) cnum ) ( (c- cnum) m )
         )
      )) n))))

(define ctwo (number->church 2))

(define c/ ; c/ (Задача 2.27)
  (lambda (m)
    (lambda (n)
      (is-czero ( (crem n) m))
      )
    ))

(define cprime-iter
  (lambda (n)
    (csnd ( ( (cp (cp n) ) (lambda (p)
           ( ( ( (c/ (cfst p)) n)
             ( (cpair (cfst p)) cfalse)
         
         )
             ( (cpair (cs (cfst p)) ) ctrue)
             ))) 
      ((cpair ctwo) ctrue)
    ))))

(define cprime ; cprime (Задача 2.27)
  (lambda (n)
    ( (( (c= n) czero) cfalse) ( ( ( (c= n) cone) cfalse) (cprime-iter n)) )
    ))

(define cexp2 ; cexp (Задача 2.20)
  (lambda (cnum1)
    (lambda (cnum2)
      ( (cnum2 (lambda (f) ( (c* cnum1) f) )) cone)
      )))

(define chyp ; chyp (Задача 2.20)
  (lambda (cnum1)
    (lambda (cnum2)
      ( (cnum2 (lambda (f) ( (cexp2 cnum1) f) )) cone)
      )))

(provide (all-defined-out))
