#lang racket
(provide (all-defined-out))
;*********** TDA Jugador **********

;********* Constructor *********

(define(player user)
  (list user (list) 0 0))


;********* Selectores ********

(define(getName u)
  (car u))

(define(getCartasUser p)
  (cadr p))

(define(getTurno u)
  (caddr u))

(define(getPuntos u)
  (cadddr u))

(define(getUser L)
  (car L))

; ******* Modificadores ********

(define(setCardsUser L c)
  (if(list? L)
     (list (getName L) c (getTurno L) (getPuntos c)(getPuntos L))
     L))

(define(setTurno L e)
     (list (getName L) (getCartasUser L) e (getPuntos L)))

(define(setPuntos L e)
  (list (getName L) (getCartasUser L) (getTurno L) e))

; ******* Otras Funciones *******

(define(registerUser L u)
  (if(empty? L)
     (cons u null)
     (if(existe? L u)
        L
     (cons (car L) (registerUser (cdr L) u)))))

(define(existe? L u)
 (if(empty? L)
     #f
     (if(equal? (getName(getUser L)) (getName u))
        #t
     (existe? (cdr L) u)))
  )
; ****

(define(Turn users i)
  (define(Faux users i Laux)
  (cond
    [(empty? users) (Faux (actualizarF Laux) 0 Laux)]
    [(= i (getTurno (getUser users)))(setTurno (getUser users) 1)]
    [else (Faux (cdr users) i Laux)]
   )
  )
  (Faux users i users)
  )

(define(actualizarT users u)
  (if(equal? (getName(getUser users)) u)
     (cons (setTurno (getUser users) 1) (cdr users))
     (cons (car users) (actualizarT (cdr users) u))))

(define(actualizarF users)
  (if(empty? users)
     null
  (cons(setTurno(getUser users) 0) (actualizarF (cdr users)))))