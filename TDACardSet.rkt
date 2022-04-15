#lang racket
(require "Mazo.rkt")
(require "TDACartas.rkt")
(provide (all-defined-out))


; Se define la funcion random que servira para la aleatoriedad de los mazos
;
;
;
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)

; Creacion del constructor del TDA CardsSet
(define (cardsSet Elements numE maxC rndFn)
         (if(<= maxC 0)
            (Mazo (- numE 1))
            (MaxMazo (Mazo (- numE 1)) maxC))
  )
  
  
  
; Creacion de la funcion de pertenencia del TDA cardsSet
(define(dobble? cards)
  (define (Faux cards Laux Laux2 i)
  (cond
    [ (> i (length Laux2)) #f]
    [ (empty? cards) #t]
    [ (empty? Laux) (Faux (nextCards cards) Laux2 Laux2 i) ]
    [(> (length(set-intersect (firstCard cards) (firstCard Laux))) 1) (Faux cards (nextCards Laux) Laux2 (+ i 1))]
    [else (Faux cards (nextCards Laux) Laux2 i)]
   )
  )
  (Faux cards cards cards 0)
  )


(define(numCards cards)
  (if (dobble? cards)
     (length cards)
     null))


(define(nthCard cards n)
  (if(dobble? cards)
     (if (= n 1)
         (firstCard cards)
     (nthCard (nextCards cards) (- n 1)))
  null)
  )


(define(findTotalCards card)
  (length (cardsSet null (length card) -1 randomFn))
  )

(define(requiredElements card)
  (findTotalCards card))


(define(missingCards cards)
  (set-subtract (cardsSet null(length(firstCard cards)) -1 null) cards)
    )


(define(cardsSet->string cards)
  (define(Faux cards L1 i)
    (if(dobble? cards)
       (if(empty? L1)
          null
          )

     "El conjunto no es valido")

  )
)

(define Ejemplo1(cardsSet null 3 4 randomFn))
(define E2(dobble? Ejemplo1))
(define E3(numCards Ejemplo1))
(define E4(nthCard Ejemplo1 3))
(define E5(findTotalCards E4))
(define E6(requiredElements E4))
(define E7(missingCards Ejemplo1))
