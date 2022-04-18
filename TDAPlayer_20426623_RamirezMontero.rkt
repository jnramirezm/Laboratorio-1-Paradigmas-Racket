#lang racket
(provide (all-defined-out))
;*********** TDA Jugador **********

;********* Constructor *********
; Descripcion: Funcion constructora del jugador
; Dominio: string (nombre del jugador)
; Recorrido: lista (contiene los elementos necesarios para que se jugador)
; Tipo de Recursividad: No

(define(player user)
  (list user (list) 0 0))


;********* Selectores ********
; Descripcion: Funcion que obtiene el nombre del jugador
; Dominio: jugador
; Recorrido: string
; Tipo de Recursividad: No

(define(getName u)
  (car u))

; Descripcion: Funcion que obtiene las cartas pertenecientes al jugador
; Dominio: jugador
; Recorrido: Lista de cartas
; Tipo de Recursividad: No

(define(getCartasUser p)
  (cadr p))

; Descripcion: Funcion que obtiene el turno del jugador (0 y 1 representando false o true)
; Dominio: jugador
; Recorrido: entero (representa un boolean)
; Tipo de Recursividad: No

(define(getTurno u)
  (caddr u))

; Descripcion: Funcion que obtiene los puntos del jugador
; Dominio: jugador
; Recorrido: entero
; Tipo de Recursividad: No

(define(getPuntos u)
  (cadddr u))

; Descripcion: Funcion que se encarga de encontrar un usuario dentro de una lista de un jugador
; Dominio: lista de jugador
; Recorrido: usuario
; Tipo de Recursividad: No

(define(getUser L)
  (car L))

; ******* Modificadores ********

; Descripcion: Funcion que retorna una version actualizada de las cartas del jugador
; Dominio: jugador x listas de cartas
; Recorrido: jugador con cartas actualizadas
; Tipo de Recursividad: No

(define(setCardsUser L c)
  (if(list? L)
     (list (getName L) c (getTurno L) (getPuntos c)(getPuntos L))
     L))

; Descripcion: Funcion que actualiza el turno del jugador
; Dominio: jugador x int (0 o 1)
; Recorrido: jugador con turno actualizado
; Tipo de Recursividad: No

(define(setTurno L e)
     (list (getName L) (getCartasUser L) e (getPuntos L)))

; Descripcion: Funcion que actualiza los puntos del jugador
; Dominio: jugador x int
; Recorrido: jugador con puntos actualizados
; Tipo de Recursividad: No

(define(setPuntos L e)
  (list (getName L) (getCartasUser L) (getTurno L) e))

; ******* Otras Funciones *******

; Descripcion: Funcion que registra a un jugador unico en una Lista de usuario, si el jugador ya esta registrado
; no lo registra
; Dominio: Lista de jugadores x jugador
; Recorrido: Lista de jugadores actualizada
; Tipo de Recursividad: Natural

(define(registerUser L u)
  (if(empty? L)
     (cons u null)
     (if(existe? L u)
        L
     (cons (car L) (registerUser (cdr L) u)))))

; Descripcion: Funcion que busca si el usuario ya existe en la lista, retorna true si es que existe, si no exite False
; Dominio: Lista de jugadores x jugador
; Recorrido: boolean
; Tipo de Recursividad: Natural

(define(existe? L u)
 (if(empty? L)
     #f
     (if(equal? (getName(getUser L)) (getName u))
        #t
     (existe? (cdr L) u)))
  )

; Descripcion: Funcion que encuentra el Usuario que le toca su turno, actualizando el estado de turno
; Dominio: Lista de jugadores x 0  (encuentra el estado 0 que seria false (no ha realizado su turno))
; Recorrido: jugador con turno actualizado
; Tipo de Recursividad: Natural
; Faux
; Dom: Lista de jugadores x 0 x Lista de jugadores (Lista inamovible)
; Recorrido: jugador con turno actualizado

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

; Descripcion: Funcion que actualiza el turno de un jugador, pero retorna toda lista de jugadores actualizada
; y si todos los jugadores realizaron su turno resetea los 1 con 0 (osea reinicia los turnos de todos a false)
; Dominio: Lista de jugadores x jugador
; Recorrido: Lista de jugadores actualizada con el jugador que hizo el turno
; Tipo de Recursividad: Natural

(define(actualizarT users u)
  (if(ETurno? users)
      (if(equal? (getName(getUser users)) u)
      (cons (setTurno (getUser users) 1) (cdr users))
      (cons (car users) (actualizarT (cdr users) u)))
      (actualizarT (actualizarF users) u))
  )

; Descripcion: Funcion que actualiza los turnos de todos los jugadores a 0
; Dominio: Lista de jugadores
; Recorrido: Lista de jugadores actualizada
; Tipo de Recursividad: Natural

(define(actualizarF users)
  (if(empty? users)
     null
  (cons(setTurno(getUser users) 0) (actualizarF (cdr users)))))

; Descripcion: Funcion que verifica si existe un turno entre los jugadores, retorna un boolean
; Dominio: Lista de jugadores
; Recorrido: boolean
; Tipo de Recursividad: Natural

(define(ETurno? users)
  (cond
    [(empty? users)#f]
    [(= 0 (getTurno(getUser users))) #t]
    [else (ETurno? (cdr users))]
    )
  )

; Descripcion: Funcion que actualiza los puntos de un jugador
; Dominio: Lista de jugadores x jugador
; Recorrido: Lista de jugadores actualizada con el jugador dado aumentado su puntaje
; Tipo de Recursividad: Natural

(define(actualizarPuntos users u)
  (if(not(empty? users))
  (if(equal? (getName(getUser users))(getName u))
     (cons(setPuntos u (+(getPuntos u)1))(cdr users))
     (cons (car users) (actualizarPuntos (cdr users) u)))
   (cdr users))
)

; Descripcion: Funcion que verifica si solo existe 1 ganador en el modo de juego StackMode
; Dominio: Lista de jugadores
; Recorrido: boolean
; Tipo de Recursividad: Natural
; Faux
; Descripcion: Si el contador aumenta de 1, es que existe un empate entre 1 o mas jugadores haciendo asi que sea F  el que solo exista 1 ganador.
; Dom; Lista de jugadores x 0 (contador) x Lista de jugadores (inmubate para no cambiar el maximo puntaje)
; recorrido: Boolean

(define(esGanador? users)
  (define(Faux users i Laux)
  (cond
    [(> i 1) #f]
    [(empty? users) #t]
    [(= (apply max (LPuntos Laux)) (getPuntos(getUser users))) (Faux (cdr users) (+ i 1) Laux)]
    [else (Faux (cdr users) i Laux)]
    )
   )
  (Faux users 0 users))

; Descripcion: Funcion que retorna una Lista con los puntos de los usuarios
; Dominio: Lista de jugadores
; Recorrido: Lista de los puntos de todos los jugadores
; Tipo de Recursividad: Natural

(define(LPuntos users)
  (if(empty? users)
     null
  (cons (getPuntos(getUser users)) (LPuntos (cdr users)))))

; Descripcion: Funcion que retorna a los ganadores o ganador del modo de juego StackMode
; Dominio: Lista de jugadores
; Recorrido: Lista el nombre del ganador / ganadores
; Tipo de Recursividad: Natural

(define(Ganador users)
  (define(Faux users Laux)
  (if(not(empty? users))
  (if(=(getPuntos(getUser users))(apply max (LPuntos Laux))) ; Se compara el puntaje del jugador con el maximo puntaje de entre todos los jugadores
     (cons (getName(getUser users))(Faux (cdr users) Laux))
     (Faux (cdr users) Laux))
  null)
  )
  (Faux users users))

; Descripcion: Funcion que retorna a los Perdedores del modo de juego StackMode
; Dominio: Lista de Jugadores
; Recorrido: Lista con los nombres de los jugadores perdedores
; Tipo de Recursividad: Natural

(define(Perdedores users)
  (define(Faux users Laux)
  (if(not(empty? users))
  (if(not(=(getPuntos(getUser users))(apply max (LPuntos Laux))))
     (cons (getName(getUser users))(Faux (cdr users) Laux))
     (Faux (cdr users) Laux))
  null)
  )
  (Faux users users))

; Descripcion: Funcion que busca a un jugador y obtiene su puntaje
; Dominio: Lista de jugadores x jugador
; Recorrido: int ( Puntaje )
; Tipo de Recursividad: Natural

(define(buscarUserP users user)
  (if(empty? users)
     null
     (if(equal?(getName(getUser users)) user)
        (getPuntos(getUser users))
        (buscarUserP (cdr users) user))))