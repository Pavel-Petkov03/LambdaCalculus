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
           (cs x)
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
       cnil)
    )))

(define (church->list clist)
  ((clist 
     (lambda (h)
       (lambda (t)
         (cons (church->number h) t))
       ))
   '()))
(define cfrom-cto ; дава числата от cstart + 1 до cend включително
  (lambda (cstart)
    (lambda (cend)
      ( cfst ( ( ( (c- cend) cstart) (lambda (f) ( (cpair ( (ccons (csnd f)) (cfst f))) (cp (csnd f))) )) ( (cpair cnil) cend)))
    )))

(define test-clist ( (cfrom-cto (number->church 55)) (number->church 70)))
; (church->list test-clist) '(56 57 58 59 60 61 62 63 64 65 66 67 68 69 70)
(define incremented-test-list ( (cmap cs ) test-clist))
; (church->list incremented-test-list) '(57 58 59 60 61 62 63 64 65 66 67 68 69 70 71)
(define filtered-less-than-65 ( (cfilter (lambda (n) ( (c< n) (number->church 65)))) test-clist) )
; (church->list filtered-less-than-65)) '(56 57 58 59 60 61 62 63 64)
(define added-nums ( ( (cfoldr c+) czero) test-clist) )
; (church->number added-nums) 945
(define concatenate-same ( (cappend test-clist) test-clist))
; (church->list concatenate-same) '(56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70)
(define test-clist-length (clength  test-clist))
; (church->number test-clist-length) 15
(define is-32-in-test ( ( (cmember? (number->church 32)) test-clist) c=))
(define is-64-in-test ( ( (cmember? (number->church 64)) test-clist) c=))
; (church->bool is-32-in-test) #f
; (church->bool is-64-in-test) #t

