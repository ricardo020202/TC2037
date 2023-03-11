;;Jose Ricardo Rosales Castañeda - A01709449 
;; Gamaliel Maruines Olvera - A01708746
#lang racket
;; temp_c: number -> number
(define temp_c
  (lambda (c)
    (+ (* c (/ 9 5)) 32)))
    

(temp_c 100)

;;Equipo: GAmaliel Marines A01708746 - Ricardo Rosales A01709449 

;;ejercicio 1, tarea
;; fahrenheit-to-celsius: number -> number
(define fahrenheit-to-celsius
  (lambda (f)
    (* (- f 32) (/ 5 9))))
    

(fahrenheit-to-celsius 212)

;; ejercicio 2, tarea
;;sign: number -> number
(define sign
  (lambda (x1)
    (cond
     [(< x1 0) -1]
     [(> x1 0) 1])))

(sign 2)

;; ejercicio 3, tarea
;;roots number, number, number -> number

(define roots
  (lambda (a b c)
    ( /
      (+ (- b)
         (sqrt (- (* b b) (* 4 a c))))
      (* 2 a))))

(roots 2 4 2)




;; ejercicio 4, tarea
;;bmi number, number -> number

(define bmi
  (lambda (w h)
    (cond
      [(< (/ w (* h h)) 20) "underweight"]
      [(and (<= (/ w (* h h)) 25) (>= (/ w (* h h)) 20)) "normal"]
      [(and (<= (/ w (* h h)) 30) (>= (/ w (* h h)) 25)) "obese1"]
      [(and (<= (/ w (* h h)) 35) (>= (/ w (* h h)) 30)) "obese2"]
      [(>= (/ w (* h h)) 40) "obese3"]
      )))

(bmi 45 1.7)
(bmi 55 1.5)
(bmi 76 1.7)
(bmi 81 1.6)
(bmi 120 1.6)


;; ejercicio 5, tarea
;;factorial number, number -> number
(define (factorial x)
  (if (= x 0)
	  1
	  (* x (factorial (- x 1)))))


(factorial 0)
(factorial 5)


;; ejercicio 6, tarea
;;duplicate list -> list
(define duplicate
  (lambda (lst)
    (cond
      [(empty? lst) '()]
      [else (cons (car lst)(cons (car lst)(duplicate (cdr lst))))])))
  


(duplicate '())
(duplicate '(1 2 3 4 5))
(duplicate '(a b c d e f g h))



;; ejercicio 7, tarea
;;pow number, number -> number
(define (pow x y)
  (cond
    [(= y 0) 0]
    [(= y 1) x]
    
    [else
     (* x (pow x (- y 1)))]
))


(pow 5 0)
(pow -5 3)
(pow 15 12)

;; ejercicio 8, tarea
;;fib number -> number
(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 6)
(fib 42)
(map fib (range 10))


;; ejercicio 9, tarea
;;enlist number -> number
(define enlist
  (lambda (lst)
    (cond
      [(empty? lst) '()]
      [(= (length lst) 1) (cons (cons (car lst) '()) '())]
      [else (cons (cons (car lst) '()) (enlist (cdr lst)))])))

(enlist '())
(enlist '(a b c))
(enlist '((1 2 3) 4 (5) 7 8))



;; ejercicio 10, tarea
;;positives list -> list
(define (positives lst)
  (cond
    [(empty? lst)'()]
    [(> (car lst) 0) (cons (car lst)
                           (positives (cdr lst)))]
    [else (positives (cdr lst))]))

(positives '(-1 1 2 -2))



;; ejercicio 11, tarea
;; Add List sum of elements of a list
(define add-list
  (lambda (lst sum)
    (cond
      [(empty? lst) sum]
      [(add-list (cdr lst) (+ sum (car lst)))])))
(add-list '(1 2 3 4 5) 0)

;; ejercicio 12, tarea
;; Invert pairs
(define invert-pairs
  (lambda (lst)
  (cond
    [(null? lst)'()]
    [(cons (list (cadr (car lst)) (car (car lst)))
            (invert-pairs (cdr lst)))])))
(invert-pairs '((1 2) (3 4) (5 6)))

;; ejercicio 13, tarea
;;lis-of-symbols?
(define list-of-symbols
  (lambda (lst)
    (cond
      [(empty? lst) #t]
      [(not (symbol? (car lst))) #f]
      [else (list-of-symbols (cdr lst))])))
(list-of-symbols '(1 2 3 4 5))
(list-of-symbols '(a b c d e))

;; ejercicio 14, tarea
;;swapper
(define swapper
  (lambda (a b lst)
    (cond
      [(empty? lst) '()]
      [(eq? a (car lst))(cons b (swapper a b (cdr lst)))]
      [(eq? b (car lst))(cons a (swapper a b (cdr lst)))]
      [else (cons (car lst) (swapper a b (cdr lst)))])))
(swapper 'purr 'kitty '(soft kitty warm kitty little ball of fur happy kitty sleepy kitty purr purr purr))

;; ejercicio 15, tarea
;;dot-product
(define dot-product
  (lambda (lst lst2)
    (cond
      [(empty? (or (and lst lst2) (lst) (lst2))) 0]
      [else (+ (* (car lst) (car lst2)) (dot-product (cdr lst) (cdr lst2)))])))
(dot-product '(1 2 3) '(2 4 3))



;; Recibe una lista de numeros y regresa la media artimertica
;; (average '(1 2 3 4 5)) -> 3
(define average
  (lambda (lst)
    (cond
      [(empty? lst) 0]
      [else (/ (add-list lst 0) (length lst))])))
(average '(1 2 3 4 5))

;; Recibe una lista de numeros y regresa la desviacion estandar
;; (std '(1 2 3 4 5)) -> 1.4142135623730951
(define std
  (lambda (lst)
    (define avg (average lst))
    (define (aux lst)
      (cond
        [(empty? lst) 0]
        [else (+ (expt (- (car lst) avg) 2) (aux (cdr lst)))]))
    (sqrt (/ (aux lst) (length lst)))))
(std '(1 2 3 4 5))

;; Recibe un numero y una lista y regresa una nueva lista
;; repitiendo cada elemento de la lista n veces
;; (replic 3 '(1 2 3)) -> '(1 1 1 2 2 2 3 3 3)
(define replic
  (lambda (n lst)
    (cond
      [(empty? lst) '()]
      [else (append (make-list n (car lst)) (replic n (cdr lst)))])))
(replic 3 '(1 2 3))

;; Recibe una lista de elementos y regresa una nueva lista
;; con el primer elemento 1 vez el segundo 2 el tercero 3
;; y asi sucesivamente
;; (expand '(1 2 3)) -> '(1 2 2 3 3 3)
(define (expand lst)
  (define (aux lst n)
    (cond
      ((empty? lst) '())
      (else (append (make-list n (car lst))
                    (aux (cdr lst) (+ n 1))))))
  (aux lst 1))
(expand '(1 2 3))

;; Recibe un numero y regresa su equivalente en binario
;; (binary 10) -> 1010
(define binary
  (lambda (n)
    (cond
      [(eq? 0 n) '()]
      [else (append (binary (quotient n 2)) (list (remainder n 2)))])))
(binary 10)