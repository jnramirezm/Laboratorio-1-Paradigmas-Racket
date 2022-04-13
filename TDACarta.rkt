#lang racket
; TDA Carta

; Constructor
(define Carta (lista elemento)
  (append lista (list elemento ) )
  )
; Pertenencia
(define(carta? Lista n)
  (and(list? Lista)
      (= (length Lista) n))
  )

; Selectores

(define(PElemento carta)
  (if(carta? carta)
     (car carta)
      null)
  )

(provide (all-defined-out))