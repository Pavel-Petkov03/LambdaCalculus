#lang racket
(require "main.rkt")


(define ccons ; представям го като fold за по - лесна реализация на map и foldr 
  (lambda (h)
    (lambda (t)
      (lambda (c)
        (lambda (x)
          ( (c h) ( (t c) x))
          )
        )
    )
  ))

(define chead cfst)
(define ctail csnd)
 
(define cnil
  (lambda (c)
    (lambda (x)
      x
      )
    )
  )

(define clength
  (lambda (clist)
    ( (clist
       (lambda (c)
         (lambda (x)
           (csucc x)
           )
         )
             ) czero)
    )
  )

(define cfoldr
  (lambda (f)
    (lambda (init)
      (lambda (clist)
        ((clist f) init)))))

(define cmember? ; поставяме трета фунция, която е компаратор
  (lambda (elem)
    (lambda (clist)
      (lambda (ccompare)
        ( (clist
         (lambda (h)
           (lambda (t)
             ( ( ( (ccompare h) elem) ctrue) t)
             )
         )
        ) cfalse)
            
            )
    )
  ))

(define cfilter
  (lambda (cpred)
    (lambda (clist)
      ((clist
         (lambda (h)
           (lambda (t)
             ( ( (cpred h) ( (ccons h) t)) t)
              )
           )
      ) cnil)
    )
  ))

(define cappend
  (lambda (clist1)
    (lambda (clist2)
      ((clist1 ccons) clist2))))

(define cmap
  (lambda (cconvert)
    (lambda (clist)
      ( (clist
         (lambda (h)
           (lambda (t)
             ( (ccons (cconvert h)) t)
           )
       ))
       cnil )
    )))

(define (church->list clist)
  ((clist 
     (lambda (h)
       (lambda (t)
         (cons (church->number h) t))
       ))
   '()))

(define cl ((ccons cone) ((ccons (number->church 15)) cnil)))
