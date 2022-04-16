#lang racket
; TDA Jugador

; Constructor

(define(player user)
  (list user (list)))

; Selectores
(define(getName u)
  (car user))

(define(getCartasUser p)
  (cadr p))

