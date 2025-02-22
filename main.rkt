#lang scheme


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
      ((ctrue cpred1) cpred2)
      )))
(define cnot
  (lambda (cpred)
    (cfalse cpred)
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


(define isczero
  (lambda (cnum)
    (cnum (ctrue cfalse) ctrue)
  ))


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
    (p (lambda (x) (lambda (y) x)))
  ))

(define csnd
  (lambda (p)
    (p (lambda (x) (lambda (y) y)))
  ))


(define (church->number cnum)
  ((cnum (lambda (x) (+ 1 x))) 0)
  ) 

 
(define (number->church num)
  (if (zero? num)
      (lambda (f) (lambda (x) x))
      (lambda (f) (lambda (x) (f (number->church (- num 1))) x))
  ))

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
(define ctwo (lambda (f) (lambda (x) (f (f x)))))
(define cthree ((cadd cone) ctwo))
(church->number cone)
(church->number ctwo)
(church->number cthree)
(define csix ((cadd cthree) cthree))
(church->number ((cexp cthree) cthree))

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


        




