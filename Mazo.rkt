#lang racket

(define(fCard n)
  (define(Fx i n)
  (if (<= i(+ n 1))
          (cons i (Fx(+ i 1) n))
           null))
 (Fx 1 n))

(define (first n)
  (list(fCard n)))

(define (nCards n)
  (define (faux j k n i)
    (if ( = i n)
        null
        (cons (f1 j k n) ( faux (+ j 1) k n (+ i 1)))))
  (faux 1 1 n 0))

(define(f1 j k n)
    (if ( <= j n)
      (cons 1 (f2 j k n))
      null
      ))

(define(f2 j k n)
  (if (<= k n)
      (cons (+(* j n) k 1) (f2 j (+ k 1) n))
      null))

(define(n2Cards n)
  (define (Fn j k i n aux it)
    (if (= it n)
        null
        (append(funaux j k i n aux)( Fn j k (+ i 1) n aux (+ it 1)))))
  
  (define(funaux j k i n aux)
    (if (= aux n)
         null
         (cons (f3 j k i n) (funaux (+ j 1)k i n (+ aux 1)))))
  (Fn 1 1 1 n 0 0))


(define(f3 j k i n)
  (if(<= i n)
     (f4 j k n i)
      null))

(define(f4 j k n i)
  (if (<= j n)
      (cons (+ i 1) (f5 j k i n))
      null))

 (define (f5 j k i n)
   (if (<= k n)
       (cons (+(modulo (+(*(- i 1)(- k 1))(- j 1)) n)(+(* n (- k 1))2 n)) (f5 j (+ k 1) i n))
       null))

(define(Mazo n)
  (append(first n)(nCards n)(n2Cards n)))

(define Ejemplo(Mazo 3))

(provide (all-defined-out))
