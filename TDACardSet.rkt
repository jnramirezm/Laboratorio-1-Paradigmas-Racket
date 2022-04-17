#lang racket
(require "Mazo.rkt")
(require "TDACartas.rkt")
(provide (all-defined-out))


; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)

(define LElementos (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "x" "y" "Z" "1" "2" "3" "4""5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" ))
(define LElementos2 (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23"))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(Ec n)
  (+(+(* n n)n)1))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define (cardsSet Elements numE maxC rndFn)
  (if(<= (length Elements) (Ec (- numE 1)))
     null
        (if(<= maxC 0)
            (Elem (Mazo (- numE 1)) Elements)
            (if (> maxC (Ec (- numE 1)))
                (Elem (Mazo (- numE 1)) Elements)
                (Elem (MaxMazo (Mazo (- numE 1)) maxC) Elements)
            )       
   )
  )
  )
  
 

; Creacion de la funcion de pertenencia del TDA cardsSet
; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(dobble? cards)
  (if(and (eq? (Faux cards cards cards 0) #t)(eq?(Faux2 cards) #t)(eq? (Faux3 cards cards) #t))
     #t
     #f)
  )
  ; Verifica que solo haya 1 elemento en comun entre todas las cartas
; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:
(define (Faux cards Laux Laux2 i)
  (cond
    [ (> i (length Laux2)) #f]
    [ (empty? cards) #t]
    [ (empty? Laux) (Faux (nextCards cards) Laux2 Laux2 i) ]
    [(> (length(set-intersect (firstCard cards) (firstCard Laux))) 1) (Faux cards (nextCards Laux) Laux2 (+ i 1))]
    [else (Faux cards (nextCards Laux) Laux2 i)]
   )
  ) 
  ; Verifica que la carta no tenga el mismo elemento
; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:
(define (Faux2 cards)
    (cond
      [(empty? cards) #t]
      [(> (length (firstCard cards)) (set-count(list->set(firstCard cards)))) #f ]
      [else (Faux2 (nextCards cards))]
     )
    )
   ;
   ; Verifica que todas las cartas tengan la misma cantidad de elementos
   ;
; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:
   (define(Faux3 cards Laux)
     (cond
       [(empty? Laux) #t]
       [(not(= (length (firstCard cards))(length (firstCard Laux)))) #f]
       [else (Faux3 cards (nextCards Laux))]
       )
    )
  

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(numCards cards)
  (if (dobble? cards)
     (length cards)
     0))

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(nthCard cards n)
  (if(dobble? cards)
     (if (= n 1)
         (firstCard cards)
     (nthCard (nextCards cards) (- n 1)))
  '())
  )

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(findTotalCards c)
  (if(empty? c)
     0
  (length (Mazo (-(length c)1 ))))
  )

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(requiredElements c)
  (if(empty? c)
     0
     (findTotalCards c))
  )

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(missingCards cards)
  (set-subtract (cardsSet LElementos2 (length(firstCard cards)) -1 null) cards)
    )

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:

(define(cardsSet->string cards)
  (if(not(dobble? cards))
     " **** Ingrese un mazo valido **** \n"
     (string-append "********** DOBBLE ********* \n\n "(string-join(FCardSt cards (CString cards) 1))"\n********** FIN ************")
     ))
; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:
  (define(FCardSt cards L1 i)
   (if(and(empty? L1))
      null
      (cons (string-append "Carta " (number->string i) ": |" (car L1) "|\n")(FCardSt cards (cdr L1) (+ i 1)))))
  
; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:
(define(CString cards)
  (if(empty? cards)
       null
       (cons (string-join(firstCard cards))(CString (nextCards cards))))
  )

; Descripcion:
; Dominio:
; Recorrido:
; Tipo de Recursividad:
(define(card . cartas) cartas)

(define(addCard mazo card)
  (if(< (length mazo) 1)
     (cons card '())
  (if(dobble? (cons card mazo))
     (append mazo (list card))
     mazo)))
  

(define Ejemplo1(cardsSet LElementos 3 4 randomFn))
(define E2(dobble? Ejemplo1))
(define E3(numCards Ejemplo1))
(define E4(nthCard Ejemplo1 5))
(define E5(findTotalCards E4))
(define E6(requiredElements E4))
(define E7(missingCards Ejemplo1))
(define E8(cardsSet->string Ejemplo1))
(define E9(addCard Ejemplo1 (card "B" "E" "G")))
(define E10(addCard E9 (card "C" "E" "F")))