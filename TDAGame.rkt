#lang racket
(require "Mazo.rkt")
(require "TDACartas.rkt")
(require "TDACardSet.rkt")
(provide (all-defined-out))
; ******************  TDA Game  *********************



; *****************  Constructor  ****************

(define(game numPlayers cardsSet mode rndFn)
  (if(dobble? cardsSet)
     (list numPlayers cardsSet mode (list) (list) )
     null)
  )

; *****************  Selectores  *******************

(define(getNPlayer L)
  (car L))

(define(getMGame L)
  (cadr L))

(define(getMode L)
  (caddr L))

(define(getUsers L)
  (cadddr L))

(define(getMesa L)
   (car(cdr(cdr(cdr (cdr L)))))
  )

; ****************  Modificadores  *********************

(define(setMazo L m )
  (if(dobble? m)
     (list (getNPlayer L) m (getMode L)(getUsers L)(getMesa L))
     L))

(define(setUsers L u)
  (if(string? u)
     (list (getNPlayer L)(getMGame L)(getMode L)(cons u (getUsers L)(getMesa L)))
     L))

(define(setMesa L m)
  (if(list? m)
     (list (getNPlayer L)(getMGame L)(getMode L)(getUsers L) m)
     L))


;;;;

(define(stackMode L)
  (setMazo (FNM L)(set-subtract(getMGame L)(getMesa(FNM L)))))

(define (FNM L)
  (setMesa L (NewM (getMGame L) 2))
  )

(define(NewM L i)
    (if(= i 0)
       null
       (cons (firstCard L) (NewM (nextCards L) (- i 1))))
    )

;;;



(define G1(cardsSet LElementos 3 0 randomFn))
(define G2(game 4 G1 null randomFn))
(define G3(stackMode G2))