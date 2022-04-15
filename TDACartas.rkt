#lang racket

; Constructor
(define(Cartas L1 L2)
  append(L1 L2)
  )

; Pertenencia
(define(Cartas? L1)
  (and(list? L1)(not(empty? L1)
  )))

; Selector
(define(firstCard M)
  (if (Cartas? M)
      (car M)
     null)
  )

(define(lastCard M)
  (if (Cartas? M)
      (car(reverse M))
    null)
  )
(define(nextCard M)
  (if (Cartas? M)
      (car(cdr M))
      null)
  )
(define(nextCards M)
  (if (Cartas? M)
      (cdr M)
     null)
  )


(provide (all-defined-out))