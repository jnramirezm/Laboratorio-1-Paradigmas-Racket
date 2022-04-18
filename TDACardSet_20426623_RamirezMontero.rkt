#lang racket
(require "CardsSetConstructor_20426623_RamirezMontero.rkt")
(require "TDACartas_20426623_RamirezMontero.rkt")
(provide (all-defined-out))

; *********************** TDA CARDSSET *****************

(define rndFn shuffle) ; Funcion que randomiza las cartas del mazo


; Definiendo lista de Elementos para su uso en el constructor CardsSet

(define LElementos (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "x" "y" "Z" "1" "2" "3" "4""5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" ))

; Descripcion: Funcion con la formula n*n+n+1
; Dominio: int
; Recorrido: int
; Tipo de Recursividad: No

(define(Ec n)
  (+(+(* n n)n)1))

; **************** cardsSet Constructor *****************

; Descripcion: Funcion constructora del mazo Cardset con sus elemento y dependiendo del maxC limita el total del mazo
; Dominio: Lista de elementos (Elements) x cantidad de elementos(numE) x cantidad de cartas en el mazo(maxC) x funcion random
; Recorrido: Lista de listas (Lista de cartas / Mazo)
; Tipo de Recursividad: Natural y de Cola (dentro de Mazo y Elem)

(define (cardsSet Elements numE maxC rndFn)
  (if(<= (length Elements) (Ec (- numE 1))) ; Compara que la cantidad de elementos es la necesaria para el total del mazo
     null
        (if(<= maxC 0)
            (rndFn (Elem (Mazo (- numE 1)) Elements))
            (if (> maxC (Ec (- numE 1)))
                (rndFn(Elem (Mazo (- numE 1)) Elements))
                (rndFn(Elem (MaxMazo (Mazo (- numE 1)) maxC) Elements))))))

; ******************* cardsSet Pertenencia ******************
  
; Descripcion: Funcion que verifica si el mazo de cartas creado en cardsSet es dobble
; Dominio: cardsSet (mazo de cartas o Lista de listas)
; Recorrido: boolean
; Tipo de Recursividad: Si (Faux, Faux2 y Faux3)

(define(dobble? cards)
  (if(and (eq? (Faux cards cards cards 0) #t)(eq?(Faux2 cards) #t)(eq? (Faux3 cards cards) #t))
     #t
     #f))

; Descripcion: Verifica que solo haya 1 elemento en comun entre todas las cartas
; Dominio: cardsSet (Lista de Listas) x cardsSet x cardsSet x contador (int)
; Recorrido: boolean
; Tipo de Recursividad: Natural

(define (Faux cards Laux Laux2 i)
  (cond
    [ (> i (length Laux2)) #f]
    [ (empty? cards) #t]
    [ (empty? Laux) (Faux (nextCards cards) Laux2 Laux2 i) ]
    [(> (length(set-intersect (firstCard cards) (firstCard Laux))) 1) (Faux cards (nextCards Laux) Laux2 (+ i 1))]
    [else (Faux cards (nextCards Laux) Laux2 i)]
   )
  ) 
 
; Descripcion: Verifica que la carta no repita el mismo elemento en ella
; Dominio: cardsSet
; Recorrido: boolean
; Tipo de Recursividad: Natural

(define (Faux2 cards)
    (cond
      [(empty? cards) #t]
      [(> (length (firstCard cards)) (set-count(list->set(firstCard cards)))) #f ]
      [else (Faux2 (nextCards cards))]
     )
    )

; Descripcion: Verifica que todas las cartas tengan la misma cantidad de elementos
; Dominio: cardsSet x cardsSet
; Recorrido: boolean
; Tipo de Recursividad: Natural 

   (define(Faux3 cards Laux)
     (cond
       [(empty? Laux) #t]
       [(not(= (length (firstCard cards))(length (firstCard Laux)))) #f]
       [else (Faux3 cards (nextCards Laux))]
       )
    )

; *********** numCards **************

; Descripcion: Funcion que entrega el total de cartas del cardsSet
; Dominio: cardsSet
; Recorrido: entero con el total de cartas del cardsSet
; Tipo de Recursividad: No

(define(numCards cards)
  (if (dobble? cards)
     (length cards)
     0))

; ************** nthCard ****************

; Descripcion: Funcion que retorna la carta nth pedida
; Dominio: cardsSet x entero
; Recorrido: Carta numero n
; Tipo de Recursividad: Natural

(define(nthCard cards n)
  (if(and(dobble? cards)(< n (numCards cards)))
     (if (= n 1)
         (firstCard cards)
     (nthCard (nextCards cards) (- n 1)))
  '())
  )

; ****************** findTotalCards **************

; Descripcion: Funcion que retorna la cantidad total de cartas que se pueden crear con una carta dada
; Dominio: Carta (Lista con elementos)
; Recorrido: entero con la cantidad total de cartas que se pueden generar con esa carta
; Tipo de Recursividad: No

(define(findTotalCards c)
  (if(empty? c)
     0
  (length (Mazo (-(length c)1 ))))
  )

; *************** requiredElements ***************

; Descripcion: Funcion que retorna la cantidad de elementos requeridos para hacer un mazo en su totalidad
; Dominio: Carta (Lista de elementos)
; Recorrido: entero que representa la cantidad de elementos requeridos
; Tipo de Recursividad: No


(define(requiredElements c)
  (if(empty? c)
     0
     (findTotalCards c))
  )

;****************** missingCards ********************

; Descripcion: Funcion que retorna las cartas faltantes de un cardsSet
; Dominio: cardsSet
; Recorrido: Lista de cartas faltantes del cardsSet
; Tipo de Recursividad: No

(define(missingCards cards)
  (if(dobble? cards)
  (set-subtract (cardsSet LElementos (length(firstCard cards)) -1 rndFn) cards)
    '()))

; ************** cardsSet->string *************

; Descripcion: Funcion que convierte a string el cardsSet para su luego uso. 
; Dominio: cardsSet
; Recorrido: String
; Tipo de Recursividad: Natural (FCardsSet y CString)

(define(cardsSet->string cards)
  (if(not(dobble? cards))
     " **** Ingrese un mazo valido **** \n"
     (string-append "\n********** DOBBLE ********* \n\n "(string-join(FCardSt cards (CString cards) 1))"\n********** FIN ************"))
  )

; Descripcion: Funcion que agrega en la Lista los elementos necesarios para decoracion
; Dominio: cardsSet x (Lista de cartas convertidas a String) x contador
; Recorrido: Lista de string de cartas a disposicion de display
; Tipo de Recursividad: Natural

  (define(FCardSt cards L1 i)
   (if(and(empty? L1))
      null
      (cons (string-append "Carta " (number->string i) ": |" (car L1) "|\n")(FCardSt cards (cdr L1) (+ i 1)))))
  
; Descripcion: Funcion que convierte cada carta a String
; Dominio: cardsSet
; Recorrido: lista de cartas en string
; Tipo de Recursividad: Natural
(define(CString cards)
  (if(empty? cards)
       null
       (cons (string-join(firstCard cards))(CString (nextCards cards))))
  )

; ************ addCard **********

; Descripcion: Funcion que anade una carta a un mazo, si es que cumple las reglas de dobble?
; Dominio: cardsSet x carta
; Recorrido: cardsSet actualizado si la carta cumple las condiciones
; Tipo de Recursividad: No

(define(card . cartas) cartas)
(define(addCard mazo card)
  (if(< (length mazo) 1)
     (cons card '())
  (if(dobble? (cons card mazo))
     (append mazo (list card))
     mazo)))
  