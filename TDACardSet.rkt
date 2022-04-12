#lang racket
(require "Mazo.rkt")
(provide (all-defined-out))


; Se define la funcion random que servira para la aleatoriedad de los mazos
;
;
;
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)
(define (prime? n)
 (let loop ((d 2))
   (cond ((< n (* d d)) #t)
         ((zero? (modulo n d)) #f)
         (else (loop (+ d 1))))))


; Creacion del constructor del TDA CardsSet
(define (cardsSet Elements numE maxC rndFn )
      (if(prime? (- numE 1))
         (if(<= maxC 0)
            (Mazo (- numE 1))
            null)
         null)
  )
  
; Creacion de la funcion de pertenencia del TDA cardsSet
(define(dobble? cards)
  cards)

(define Ejemplo1(cardsSet null 3 0 randomFn))
(define E2(dobble? Ejemplo1))
