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

(define LElementos (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q"))
(define LElementos2 (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23"))

(define(Ec n)
  (+(+(* n n)n)1))

; Creacion del constructor del TDA CardsSet
;
;
;
;

(define (cardsSet Elements numE maxC rndFn)
  (if(< (length Elements) (Ec (- numE 1)))
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
;
;
;
;

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

;
;
;
;
;

(define(numCards cards)
  (if (dobble? cards)
     (length cards)
     null))

;
;
;
;
;

(define(nthCard cards n)
  (if(dobble? cards)
     (if (= n 1)
         (firstCard cards)
     (nthCard (nextCards cards) (- n 1)))
  null)
  )

;
;
;
;
;

(define(findTotalCards card)
  (if(empty? card)
     null
  (length (Mazo (-(length card)1 ))))
  )

;
;
;
;
;

(define(requiredElements card)
  (if(empty? card)
     null
     (findTotalCards card))
  )

;
;
;
;
;
;

(define(missingCards cards)
  (set-subtract (cardsSet LElementos2 (length(firstCard cards)) -1 null) cards)
    )

;
;
;
;
;

(define(cardsSet->string cards)
  (define(Faux cards L1 i)
   (if(empty? L1)
      null
      (cons (string-append "Carta " (number->string i) ": " (car L1) "\n")(Faux cards (cdr L1) (+ i 1)))))
  (Faux cards (CString cards) 1))

(define(CString cards)
  (if(empty? cards)
       null
       (cons (string-join(firstCard cards))(CString (nextCards cards))))
  )



(define Ejemplo1(cardsSet LElementos2 3 4 randomFn))
(define E2(dobble? Ejemplo1))
(define E3(numCards Ejemplo1))
(define E4(nthCard Ejemplo1 5))
(define E5(findTotalCards E4))
(define E6(requiredElements E4))
(define E7(missingCards Ejemplo1))
(define E8(cardsSet->string Ejemplo1))