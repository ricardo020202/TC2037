#lang racket
;;La funci ́on add-list devuelve la suma de los
;;numeros contenidos en la lista que recibe como entrada,
;;o 0 si est́a vaćıa.
;;(add-list '(1 2 3 4 5) 0) -> 15
(define add-list
  (lambda (lst sum)
    (cond
      [(empty? lst) sum]
      [(add-list (cdr lst) (+ sum (car lst)))])))
(add-list '(1 2 3 4 5) 0)

;; Recibe una lista de sublistas formadas de pares y
;; regresa una nueva lista pero con las sublistas invertidas
;; (invert-pairs '((a 1)(a 2)(b 1)(b 2))) -> ((1 a)(2 a)(1 b)(2 b))
(define invert-pairs
  (lambda (lst)
  (cond
    [(null? lst)'()]
    [(cons (list (cadr (car lst)) (car (car lst)))
            (invert-pairs (cdr lst)))])))
(invert-pairs '((a 1) (a 2) (a 3)))

;; Recibe una lista de elementos y regresa true
;; si la lista contiene puros simbolos
;; (list-of-symbols '(1 2 3 4 5)) -> #f
;; (list-of-symbols '(a b c d e)) -> #t
(define list-of-symbols
  (lambda (lst)
    (cond
      [(empty? lst) #t]
      [(not (symbol? (car lst))) #f]
      [else (list-of-symbols (cdr lst))])))
(list-of-symbols '(1 2 3 4 5))
(list-of-symbols '(a b c d e))

;; Recibe una lista de elementos y 2 elementos (a, b)
;; y regresa unas nueva lista donde cambia todas las
;; instancias de a por b y viceversa
;; (swapper a b '(a b b a a)) -> '(b a a b b)
(define swapper
  (lambda (a b lst)
    (cond
      [(empty? lst) '()]
      [(eq? a (car lst))(cons b (swapper a b (cdr lst)))]
      [(eq? b (car lst))(cons a (swapper a b (cdr lst)))]
      [else (cons (car lst) (swapper a b (cdr lst)))])))
(swapper 'purr 'kitty '(soft kitty warm kitty little ball of fur happy kitty sleepy kitty purr purr purr))

;; Recibe dos listas y regresa el producto punto de estas 2
;; (dot-product '(1 2 3) '(4 5 6)) -> 32
(define dot-product
  (lambda (lst lst2)
    (cond
      [(empty? (or (and lst lst2) (lst) (lst2))) 0]
      [else (+ (* (car lst) (car lst2)) (dot-product (cdr lst) (cdr lst2)))])))
(dot-product '(1 2 3) '(4 5 6))

;; Recibe una lista de numeros y regresa la media artimertica
;; (average '(1 2 3 4 5)) -> 3
(define average
  (lambda (lst)
    (cond
      [(empty? lst) 0]
      [else (/ (add-list lst 0) (length lst))])))
(average '(1 2 3 4 5))

;;Standard deviation

;;Replic

;;Expand

;;Binary
