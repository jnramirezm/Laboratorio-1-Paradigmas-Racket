#lang racket
(require "cardsSetConstructor_20426623_RamirezMontero.rkt")
(require "TDACartas_20426623_RamirezMontero.rkt")
(require "TDACardSet_20426623_RamirezMontero.rkt")
(require "TDAPlayer_20426623_RamirezMontero.rkt")
(provide (all-defined-out))
; ******************  TDA Game  *********************



; *****************  Constructor  ****************

; Descripcion: Funcion que construye el juego
; Dominio: numPlayer(Int) x cardsSet (list) x mode x funcion randomizadora
; Recorrido: Game (lista que contiene todo lo necesario para el funcionamiento del juego)
; Tipo de Recursividad: No

(define(game numPlayers cardsSet mode rndFn)
  (if(dobble? cardsSet)
     (list  numPlayers cardsSet  mode (list) (list) 0 "" )
     null)
  )

; Descripcion: Funcion constructora de game pero utilizando parametros dados
; Dominio: numPlayer x cardsSet x mode x ListaUsers x mesa (lista) x estado de partida  x string (Mensaje al final de la partida)
; Recorrido: game
; Tipo de Recursividad: No

(define(gameC numPlayers cardsSet mode users mesa e s)
  (list numPlayers cardsSet mode users mesa e s))

; *****************  Selectores  *******************
; Descripcion: Funcion que obtiene la cantidad de jugadores de la partida
; Dominio: Lista (game)
; Recorrido: jugadores totales de la partida (int)
; Tipo de Recursividad: No

(define(getNPlayers L)
  (car L))

; Descripcion: Funcion que obtiene el mazo del juego
; Dominio: game
; Recorrido: Mazo (cardsSet)
; Tipo de Recursividad: No

(define(getMGame L)
  (cadr L))

; Descripcion: Funcion que obtiene el modo del juego
; Dominio: game
; Recorrido: procedure
; Tipo de Recursividad: No

(define(getMode L)
  (caddr L))

; Descripcion: Funcion que obtiene los Usuarios del juego
; Dominio: game
; Recorrido: usuarios del juego (lista de listas)
; Tipo de Recursividad: No

(define(getUsers L)
  (cadddr L))

; Descripcion: Funcion que obtiene la mesa del juego (cartas que estan dadas vueltas)
; Dominio: game
; Recorrido: lista de cartas en la mesa
; Tipo de Recursividad: No

(define(getMesa L)
   (car(cdr(cdr(cdr (cdr L))))))

; Descripcion: Funcion que obtiene el estado del juego (0 o 1) que funcionan como boolean, si es 0 el juego continua,
; si es 1 el juego esta terminado
; Dominio: game
; Recorrido: int (que funciona como boolean)
; Tipo de Recursividad: No

(define(getEstado L)
  (car(cdr(cdr(cdr(cdr(cdr L)))))))

; Descripcion: Funcion que obtiene el mensaje del terminado del juego con ganadores/empate/perdedores
; Dominio: game
; Recorrido: string
; Tipo de Recursividad: No

(define(getFin L)
  (car(cdr(cdr(cdr(cdr(cdr (cdr L)))))))
  )

; ****************  Modificadores  *********************

; Descripcion: Funcion que se encarga de crear una nueva version del juego con el mazo actualizado
; Dominio: game x mazo (cardsSet actualizado)
; Recorrido: game actualizado
; Tipo de Recursividad: No

(define(setMazo L m )
  (if(dobble? m)
     (list (getNPlayers L) m (getMode L)(getUsers L)(getMesa L)(getEstado L)(getFin L))
     L))

; Descripcion: Funcion que se encarga de crear una nueva version del juego con los Jugadores actualizados
; Dominio: game x players (lista de usuarios)
; Recorrido: game
; Tipo de Recursividad: No

(define(setUsers L u)
  (if(list? u)
     (list (getNPlayers L)(getMGame L)(getMode L) u (getMesa L)(getEstado L)(getFin L))
     L))

; Descripcion: Funcion que se encarga de crear una nueva version del juego con la mesa actualizada
; Dominio: game x mesa (Lista de cartas)
; Recorrido: game
; Tipo de Recursividad: No

(define(setMesa L m)
  (if(list? m)
     (list (getNPlayers L)(getMGame L)(getMode L)(getUsers L) m (getEstado L)(getFin L))
     L))

; Descripcion: Funcion que se encarga de crear una nueva version del juego con el estado actualizado, para finalizar el juego
; Dominio: game x int (0 o 1)
; Recorrido: game
; Tipo de Recursividad: No

(define(setEstado L e)
  (if(integer? e)
     (list (getNPlayers L)(getMGame L)(getMode L)(getUsers L) (getMesa L) e (getFin L))
     L))

; Descripcion: Funcion que se encarga de crear una nueva version del juego, para cuando el juego finaliza tener
; el mensaje con los ganadores/perdedores
; Dominio: game x string
; Recorrido: game
; Tipo de Recursividad: No

(define(setFin L f)
  (if(string? f)
     (list (getNPlayers L)(getMGame L)(getMode L)(getUsers L) (getMesa L) (getEstado L) f)
     L))

; **************    Otras Funciones  ***************


;  **************** StackMode **********

; Descripcion: Funcion que saca 2 cartas del Mazo para dejarlas sobre la mesa
; Dominio: game ( Lista )
; Recorrido: game actualizado
; Tipo de Recursividad: No

(define(stackMode L)
  (setMazo (FNM L)(set-subtract(getMGame L)(getMesa(FNM L)))))

; Descripcion: Funcion que actualiza con 2 cartas sobre la mesa
; Dominio: game ( Lista )
; Recorrido: game actualizado
; Tipo de Recursividad: No

(define (FNM L)
  (setMesa L (NewM (getMGame L) 2))
  )
; Descripcion: Funcion quita 2 cartas al mazo
; Dominio: Mazo x int
; Recorrido: Lista de 2 cartas
; Tipo de Recursividad: Natural

(define(NewM L i)
    (if(= i 0)
       null
       (cons (firstCard L) (NewM (nextCards L) (- i 1))))
    )

; *********** Register Jugador ********

; Descripcion: Funcion que registra a un usuario unico, no registra si el usuario ya existe o si el total de jugadores
; ya se cumplio
; Dominio: game x usuario ( String )
; Recorrido: game actualizado con el usuario creado si existe / o devuelve game sin actualizar si no pudo registrar
; Tipo de Recursividad: Natural ( en RegisterUser )

(define(register game user)
  (if(and(string? user)(list? game)(< (length (getUsers game)) (getNPlayers game)))
     (setUsers game (registerUser (getUsers game) (player user)))
     game)
  )

; ************ whoseTurnIsIt? ****************

; Descripcion: Funcion que retorna el nombre del jugador que le pertence el turno
; Dominio: game
; Recorrido: string
; Tipo de Recursividad: Natural ( ETurno -> Natural) ( ActualizarF -> Natural / actualiza los 1 por 0 si ETurno? no encuentra un turno)

(define(whoseTurnIsIt? game)
    (if(not(empty? (getUsers game)))
      (cond
         [(ETurno? (getUsers game))(getName (Turn (getUsers game)0)) ]
         [else (getName(getUser(getUsers(setUsers game (actualizarF (getUsers game)))))) ]
        )
     ""))
; **************** Play **********************

; Descripcion: Funcion que realiza las jugadas de los jugadores respecto a la accion realizada tal como pass/spotIt y
; acciones del juego tales como null que aplica StackMode y finish que termina el juego
; Dominio: game x action (procedure) x rndFn (funcion random)
; Recorrido: game actualizado dependiendo de la accion recibida
; Tipo de Recursividad: Cola ( en pass y finish) y Natural en SpotIt (En otras funciones llamadas se hacen llamado a diferente recursiones igualmente)

(define(play game action rndFn)
  (if(not(= (getEstado game)1))
     (if(equal? action null)
        (stackMode game)
        (if(string? action)
          (spotItR game action)
          (action game)))
  game))

; **************************** Pass **********************

; Descripcion: Funcion pass, el jugador hace uso de su turno y la mesa se vacia.
; se hace uso de una funcion auxiliar para mayor claridad en los cambios dentro del game.
; Dominio: game
; Recorrido: game actualizado, con la mesa vacia y el jugador que utilizo su turno para pasar.
; Tipo de Recursividad: Natural (dentro de ActualizarT y whoseTurnIsIt?)

(define(pass game)
   (define(Faux game aux Laux2 Laux3)
     (list (getNPlayers game) Laux2 (getMode game) aux Laux3 (getEstado game)(getFin game)))
  (Faux game (actualizarT (getUsers game)(whoseTurnIsIt? game))(append (getMGame game)(getMesa game))'())
  )

; **************************** SpotIt *********************

; Descripcion: Funcion Spotit, el jugador que acierte en la carta con el mismo elemento se le aumenta su puntaje
; y aumenta su turno, en cambio si no acierta igualmente aumenta su turno pero no aumenta su puntaje. Si acierta se
; limpia la mesa, actualizando el game.
; Dominio: game x SpotIt x n
; Recorrido: game 
; Tipo de Recursividad: Natural (whoseTurnIsIt?, actualizarPuntos, actualizarF)

(define (spotItR game n)
  (if(equal?(car(set-intersect(car (getMesa game))(cadr (getMesa game)))) n)
     (if(ETurno?(getUsers game))
         (setMesa(setUsers game(actualizarPuntos (getUsers game)(Turn (getUsers game)0)))'())
        (setMesa(setUsers game (actualizarPuntos (getUsers game)(Turn (actualizarF (getUsers game))0)))'()))
     (setUsers game (actualizarT (getUsers game)(whoseTurnIsIt? game))))
  )
(define(spotIt n)
  n)

; ****************************** Finish *******************************

; Descripcion: Funcion que actualiza el estado del game, terminando asi el juego retornando los jugadores
; perdedores y ganador, o ganadores si resulta un empate
; Dominio: game
; Recorrido: game actualizado, con 1 en el estado representando el terminado y con los ganadores/perdedores en el texto final
; Tipo de Recursividad: Natural ( esGanador? para encontrar a los jugadores/jugador que gano y Fn Ganador.)
; Faux
; Dom: game x Texto Ganador (list de string) x Texto Empate (list de String)
; Rec: game actualizado dependiendo si hubo ganadores o ganador.

(define(finish game)
  (define(Faux game F1 F2)
  (if(esGanador? (getUsers game))
     (list (getNPlayers game) (getMGame game) (getMode game) (getUsers game) (getMesa game) 1 F1)
     (list (getNPlayers game) (getMGame game) (getMode game) (getUsers game) (getMesa game) 1 F2))
       )
  (Faux game (append '("Ganador:")(Ganador (getUsers game))'("Perdedores:")(Perdedores(getUsers game)))
        (append '("Empate:") (Ganador (getUsers game))'("Perdedores:") (Perdedores(getUsers game)))))

; *************** Status ***************

; Descripcion: Funcion que retorna el status del juego
; Dominio: game
; Recorrido: string
; Tipo de Recursividad: No

(define(status game)
  (if(= (getEstado game)0)
     "El Juego esta en proceso"
     "El Juego ha Terminado"))

; ************** Score ***************

; Descripcion: Funcion que retorna el puntaje de un jugador dado
; Dominio: game x string (nombre jugador)
; Recorrido: int (puntaje)
; Tipo de Recursividad: Natural (buscarUserP)

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

; ******************************* EJEMPLOS ***************************************

