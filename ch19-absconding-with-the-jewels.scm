(define (deep m)
  (if (zero? m)
      'pizza
      (cons (deep (sub1 m)) '())))

(deep 6)

;; naive procedure to define n-layers of pizza with different ingredients.
(define (six-layers p)
  (cons
   (cons
    (cons
     (cons
      (cons
       (cons p '())
       '())
      '())
     '())
    '())
   '()))

(six-layers 'pizza)
(six-layers 'mozzarella)

(define (four-layers p)
  (cons
   (cons
    (cons
     (cons p '())
     '())
    '())
   '()))

(four-layers 'pizza)

(define toppings)
(define (deepB m)
  (if (zero? m)
      (call/cc
       (lambda (jump)
         (set! toppings jump)   ;; save the `jump', which seems to be some function
         'pizza))
      (cons (deepB (sub1 m)) '())))

(deepB 6)

;; Wow! So `toppings` remember its stacks while being called!! This is
;; mind-blowing, so I could design a function with not only its closure,
;; but also its proceeding stacks.

(toppings 'mozzarella)
(toppings 'cake)

;; This is unexpected though, `toppings' also ignore the stacks in context.
(cons (toppings 'cake) '())
(cons (cons (toppings 'cake) '()) '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The twentieth commandment                                                  ;
;                                                                            ;
; When thinking about a value created with (letcc ...), write down the       ;
; function that is equivalent but does not forget. Then, when you use it,    ;
; remember to forget.                                                        ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deepB 4) ;; set `toppings' to a four-layered
(cons (toppings 'cake)
      (toppings 'cake))

(cons
 (toppings 'cake)
 (cons
  (toppings 'mozzarella)
  (cons
   (toppings 'pizza) '())))

;; Let'S try to cons things onto `toppings'

;; redefine `deep' with a collector
(define (deepCo m k)
  (if (zero? m)
      (k 'pizza)
      (deepCo (sub1 m)
              (lambda (x)
                (k (cons x '()))))))

(deepCo 0 (lambda (x) x))
(deepCo 2 (lambda (x) x))
;; a step-by-step evaluation of the previous expression
(deepCo 1 (lambda (x)
            (cons x '())))
(deepCo 0 (lambda (x)
            ((lambda (x)  ;; NOTE: This is two-layered
               (cons x '()))
             (cons x '()))))

(define (deepCoB m k)
  (if (zero? m)
      (let ()
        (set! toppings k)  ;; Now, let's remember the collector in toppings
        (k 'pizza))
      (deepCoB (sub1 m)
              (lambda (x)
                (k (cons x '()))))))

(deepCoB 4 (lambda (x) x))

;; This version of `toppings' doesn't forget.

;; NOTE: the collector is an explicit collector of stacks and set! expose the
;; inner-most procedure to be used.
(cons
 (toppings 'cake)
 (toppings 'cake))
