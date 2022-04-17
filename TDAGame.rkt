#lang racket
(require "Mazo.rkt")
(require "TDACartas.rkt")
(require "TDACardSet.rkt")
(require "TDAPlayer.rkt")
(provide (all-defined-out))
; ******************  TDA Game  *********************



; *****************  Constructor  ****************

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(game numPlayers cardsSet mode rndFn)
  (if(dobble? cardsSet)
     (list  numPlayers cardsSet  "mode" (list) (list) 0 "" )
     null)
  )

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(gameC numPlayers cardsSet mode users mesa e s)
  (list numPlayers cardsSet mode users mesa e s))

; *****************  Selectores  *******************
; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(getNPlayers L)
  (car L))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(getMGame L)
  (cadr L))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(getMode L)
  (caddr L))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(getUsers L)
  (cadddr L))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(getMesa L)
   (car(cdr(cdr(cdr (cdr L))))))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(getEstado L)
  (car(cdr(cdr(cdr(cdr(cdr L)))))))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(getFin L)
  (car(cdr(cdr(cdr(cdr(cdr (cdr L)))))))
  )

; ****************  Modificadores  *********************

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(setMazo L m )
  (if(dobble? m)
     (list (getNPlayers L) m (getMode L)(getUsers L)(getMesa L)(getEstado L)(getFin L))
     L))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(setUsers L u)
  (if(list? u)
     (list (getNPlayers L)(getMGame L)(getMode L) u (getMesa L)(getEstado L)(getFin L))
     L))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(setMesa L m)
  (if(list? m)
     (list (getNPlayers L)(getMGame L)(getMode L)(getUsers L) m (getEstado L)(getFin L))
     L))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(setEstado L e)
  (if(integer? e)
     (list (getNPlayers L)(getMGame L)(getMode L)(getUsers L) (getMesa L) e (getFin L))
     L))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(setFin L f)
  (if(string? f)
     (list (getNPlayers L)(getMGame L)(getMode L)(getUsers L) (getMesa L) (getEstado L) f)
     L))

;;;;

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

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

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(register game user)
  (if(and(string? user)(list? game)(< (length (getUsers game)) (getNPlayers game)))
     (setUsers game (registerUser (getUsers game) (player user)))
     game)
  )

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(whoseTurnIsIt? game)
    (if(not(empty? (getUsers game)))
      (cond
         [(ETurno? (getUsers game))(getName (Turn (getUsers game)0)) ]
         [else (getName(getUser(getUsers(setUsers game (actualizarF (getUsers game)))))) ]
        )
     ""))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(play game action rndFn)
  (if(not(= (getEstado game)1))
     (if(equal? action null)
        (stackMode game)
        (if(string? action)
          (spotItR game action)
          (action game)))
  game))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(pass game)
   (define(Faux game aux Laux2 Laux3)
     (list (getNPlayers game) Laux2 (getMode game) aux Laux3 (getEstado game)(getFin game)))
  (Faux game (actualizarT (getUsers game)(whoseTurnIsIt? game))(append (getMGame game)(getMesa game))'())
  )


; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define (spotIt n)
  n)

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define (spotItR game n)
  (if(equal?(car(set-intersect(car (getMesa game))(cadr (getMesa game)))) n)
     (if(ETurno?(getUsers game))
         (setMesa(setUsers game(actualizarPuntos (getUsers game)(Turn (getUsers game)0)))'())
        (setMesa(setUsers game (actualizarPuntos (getUsers game)(Turn (actualizarF (getUsers game))0)))'()))
     (setUsers game (actualizarT (getUsers game)(whoseTurnIsIt? game))))
  )

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(finish game)
  (define(Faux game F1 F2)
  (if(esGanador? (getUsers game))
     (list (getNPlayers game) (getMGame game) (getMode game) (getUsers game) (getMesa game)  1 F1)
     (list (getNPlayers game) (getMGame game) (getMode game) (getUsers game) (getMesa game)  1 F2))
    )
  (Faux game (append '("Ganador:")(Ganador (getUsers game))'("Perdedores:")(Perdedores(getUsers game)))
        (append '("Empate:" (Ganador (getUsers game))'("Perdedores:") (Perdedores(getUsers game))))))


; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(status game)
  (string-join (map ~a game)""))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(score game user)
  (buscarUserP (getUsers game) user))

;;;
;(define(game->string game)
;  (Pstring game))
;
;(define(Pstring game)
;  (if(empty? game)
;    null
;  (cond
;      [(list? (car game))(cons(list->string (car game))(Pstring (cdr game)))]
;      [(number? (car game))(cons(number->string(car game))(Pstring (cdr game)))]
;      [(string? (car game)) (cons(car game)(Pstring(cdr game)))]
;      )))

(define G1(cardsSet LElementos 3 0 randomFn))
(define G2(game 4 G1 stackMode randomFn))
(define G3(stackMode G2))
(define G4(register G2 "Daigo" ))
(define G5(register G4 "Ale" ))
(define G6(register G5 "Juan"))
(define G7(register G6 "Daigo"))
(define G8(register G7 "Tomas"))
(define G9(register G8 "Gatito"))
;(define G10(whoseTurnIsIt? G8))
(define G11(play G9 null null))
(define G12(play G11 pass null))
(define G13(play G12 pass null))
(define G14(play G13 pass null))
(define G15(play G14 null null))
(define G16(play G15 (spotIt "C")null))
(define G17(play G16 null null))
(define G18(play G17 finish null))
(define G19(play G18 null null))
(define G20(status G16))
(define G21(score G19 "Tomas"))

