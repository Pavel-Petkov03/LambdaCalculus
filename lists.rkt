#lang scheme

(define ctrue
  (lambda (x)
    (lambda (y)
      x
      )))

(define cfalse
  (lambda (x)
    (lambda (y)
      y
      )))

(define c<>
  (lambda (x)
    (lambda (y)
      (lambda (p)
      ((p x) y)
      )
  )))

(define cleft
  (lambda (p)
    (p ctrue)))

(define cright
  (lambda (p)
    (p cfalse)))

  