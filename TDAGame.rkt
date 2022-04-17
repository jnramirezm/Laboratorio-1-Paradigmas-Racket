#lang racket
(require "Mazo.rkt")
(require "TDACartas.rkt")
(require "TDACardSet.rkt")
(require "TDAPlayer.rkt")
(provide (all-defined-out))
; ******************  TDA Game  *********************



; *****************  Constructor  ****************

(define(game numPlayers cardsSet mode rndFn)
  (if(dobble? cardsSet)
     (list numPlayers cardsSet mode (list) (list) 0 )
     null)
  )
(define(gameC numPlayers cardsSet mode users mesa e)
  (list numPlayers cardsSet mode users mesa e))

; *****************  Selectores  *******************

(define(getNPlayers L)
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
(define(getEstado L)
  (car(cdr(cdr(cdr(cdr (cdr L))))))
  )

; ****************  Modificadores  *********************

(define(setMazo L m )
  (if(dobble? m)
     (list (getNPlayers L) m (getMode L)(getUsers L)(getMesa L)(getEstado L))
     L))

(define(setUsers L u)
  (if(list? u)
     (list (getNPlayers L)(getMGame L)(getMode L) u (getMesa L)(getEstado L))
     L))

(define(setMesa L m)
  (if(list? m)
     (list (getNPlayers L)(getMGame L)(getMode L)(getUsers L) m (getEstado L))
     L))

(define(setEstado L e)
  (if(integer? e)
     (list (getNPlayers L)(getMGame L)(getMode L)(getUsers L) (getMesa L) e)
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

(define(register game user)
  (if(and(string? user)(list? game)(< (length (getUsers game)) (getNPlayers game)))
     (setUsers game (registerUser (getUsers game) (player user)))
     game)
  )

;;;

(define(whoseTurnIsIt? game)
  (if(not(empty? (getUsers game)))
       (getName (Turn (getUsers game)0))
       "")
  )

;;;

(define(play game action rndFn)
  (if(equal? action null)
     (stackMode game)
     (if(string? action)
        #t
        (action game)))
  )

(define(pass game)
  (define(Faux game aux)
     (list (getNPlayers game)(getMGame game)(getMode game)aux (getMesa game)(getEstado game)))
  (Faux game (actualizarT (getUsers game)(whoseTurnIsIt? game)))
  )



(define G1(cardsSet LElementos 3 0 randomFn))
(define G2(game 4 G1 stackMode randomFn))
(define G3(stackMode G2))
(define G4(register G2 "Daigo" ))
(define G5(register G4 "Ale" ))
(define G6(register G5 "Juan"))
(define G7(register G6 "Daigo"))
(define G8(register G7 "Tomas"))
(define G9(register G8 "Gatito"))
(define G10(whoseTurnIsIt? G8))
(define G11(play G9 null null))
(define G12(play G11 pass null))
(define G13(play G12 pass null))
(define G14(play G13 pass null))
(define G15(play G14 pass null))
(define G16(play G15 pass null))
