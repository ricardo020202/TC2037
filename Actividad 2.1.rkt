#lang racket
;; Add List sum of elements of a list
(define add-list
  (lambda (lst sum)
    (cond
      [(empty? lst) sum]
      [(add-list (cdr lst) (+ sum (car lst)))])))
(add-list '(1 2 3 4 5) 0)

;; Invert pairs
(define invert-pairs
  (lambda (lst)
  (cond
    [(null? lst)'()]
    [(cons (list (cadr (car lst)) (car (car lst)))
            (invert-pairs (cdr lst)))])))
(invert-pairs '((1 2) (3 4) (5 6)))

;;lis-of-symbols?
(define list-of-symbols
  (lambda (lst)
    (cond
      [(empty? lst) #t]
      [(not (symbol? (car lst))) #f]
      [else (list-of-symbols (cdr lst))])))
(list-of-symbols '(1 2 3 4 5))
(list-of-symbols '(a b c d e))

;;swapper
(define swapper
  (lambda (a b lst)
    (cond
      [(empty? lst) '()]
      [(eq? a (car lst))(cons b (swapper a b (cdr lst)))]
      [(eq? b (car lst))(cons a (swapper a b (cdr lst)))]
      [else (cons (car lst) (swapper a b (cdr lst)))])))
(swapper 'purr 'kitty '(soft kitty warm kitty little ball of fur happy kitty sleepy kitty purr purr purr))

;;dot-product
(define dot-product
  (lambda (lst lst2)
    (cond
      [(empty? (or (and lst lst2) (lst) (lst2))) 0]
      [else (+ (* (car lst) (car lst2)) (dot-product (cdr lst) (cdr lst2)))])))
(dot-product '(1 2 3) '(2 4 3))