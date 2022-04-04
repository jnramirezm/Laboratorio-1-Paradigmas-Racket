#lang racket

; Se define la funcion random que servira para la aleatoriedad de los mazos
;
;
;

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)

; Creacion del constructor del TDA CardsSet
(define (cardsSet elementos entero maxC randomFn )
    (list elementos entero maxC randomFN) 
)

; Creacion de la funcion de pertenencia del TDA cardsSet
(define( dobble?))