#lang racket
(provide (all-defined-out))
;*********** TDA Jugador **********

;********* Constructor *********
; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(player user)
  (list user (list) 0 0))


;********* Selectores ********
; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(getName u)
  (car u))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(getCartasUser p)
  (cadr p))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(getTurno u)
  (caddr u))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(getPuntos u)
  (cadddr u))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(getUser L)
  (car L))

; ******* Modificadores ********

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(setCardsUser L c)
  (if(list? L)
     (list (getName L) c (getTurno L) (getPuntos c)(getPuntos L))
     L))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(setTurno L e)
     (list (getName L) (getCartasUser L) e (getPuntos L)))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(setPuntos L e)
  (list (getName L) (getCartasUser L) (getTurno L) e))

; ******* Otras Funciones *******

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(registerUser L u)
  (if(empty? L)
     (cons u null)
     (if(existe? L u)
        L
     (cons (car L) (registerUser (cdr L) u)))))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(existe? L u)
 (if(empty? L)
     #f
     (if(equal? (getName(getUser L)) (getName u))
        #t
     (existe? (cdr L) u)))
  )

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(Turn users i)
  (define(Faux users i Laux)
  (cond
    [(empty? users) ""]
    [(= i (getTurno (getUser users)))(setTurno (getUser users) 1)]
    [else (Faux (cdr users) i Laux)]
   )
  )
  (Faux users i users)
  )

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(actualizarT users u)
  (if(ETurno? users)
      (if(equal? (getName(getUser users)) u)
      (cons (setTurno (getUser users) 1) (cdr users))
      (cons (car users) (actualizarT (cdr users) u)))
      (actualizarT (actualizarF users) u))
  )

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(actualizarF users)
  (if(empty? users)
     null
  (cons(setTurno(getUser users) 0) (actualizarF (cdr users)))))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(ETurno? users)
  (cond
    [(empty? users)#f]
    [(= 0 (getTurno(getUser users))) #t]
    [else (ETurno? (cdr users))]
    )
  )

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(actualizarPuntos users u)
  (if(not(empty? users))
  (if(equal? (getName(getUser users))(getName u))
     (cons(setPuntos u (+(getPuntos u)1))(cdr users))
     (cons (car users) (actualizarPuntos (cdr users) u)))
   (cdr users))
)

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(esGanador? users)
  (define(Faux users i)
  (cond
    [(> i 1) #f]
    [(empty? users) #t]
    [(= (apply max (LPuntos users)) (getPuntos(getUser users))) (Faux (cdr users) (+ i 1))]
    [else (Faux (cdr users) i)]
    )
   )
  (Faux users 0))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(LPuntos users)
  (if(empty? users)
     null
  (cons (getPuntos(getUser users)) (LPuntos (cdr users)))))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(Ganador users)
  (define(Faux users Laux)
  (if(not(empty? users))
  (if(=(getPuntos(getUser users))(apply max (LPuntos Laux)))
     (cons (getName(getUser users))(Faux (cdr users) Laux))
     (Faux (cdr users) Laux))
  null)
  )
  (Faux users users))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(Perdedores users)
  (define(Faux users Laux)
  (if(not(empty? users))
  (if(not(=(getPuntos(getUser users))(apply max (LPuntos Laux))))
     (cons (getName(getUser users))(Faux (cdr users) Laux))
     (Faux (cdr users) Laux))
  null)
  )
  (Faux users users))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(buscarUserP users user)
  (if(empty? users)
     null
     (if(equal?(getName(getUser users)) user)
        (getPuntos(getUser users))
        (buscarUserP (cdr users) user))))