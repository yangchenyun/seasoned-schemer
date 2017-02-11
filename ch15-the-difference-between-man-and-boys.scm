(define x
  (cons 'chicago
        (cons 'pizza '())))

;; `set!' is like `define'
(set! x 'gone)
x

(set! x 'skins)
x

(define gourmet
  (lambda (food)
    (cons food (cons x '()))))          ;; the value of `x' is taken from the
                                        ;; environment

(gourmet 'onion)

(set! x 'rings)

(gourmet 'onion)

(define (gourmand food)
  (set! x food) ;; This changes the binding of x in the environment as well.
  (cons food (cons x '())))

(gourmand 'potato)
x ;; is 'potato now

;; rewrite `diner' to remember the food eaten last
(define (diner food)
  (cons 'milkshake
        (cons food '())))

(define (dinerR food)
  (set! x food)
  (cons 'milkshake
        (cons food '())))

(dinerR 'onion)
x
(gourmand 'potato)
x ;; Both share the same `x' in its environment

(define omnivore
  (let ((x 'minestrone)) ;; now `x' is a local binding only visible inside `omnivore'
    (lambda (food)
      (set! x food)
      (cons food
            (cons x '())))))

(omnivore 'bouillabaisse)
x ;; remain unchanged

;; .----------------------------------------------------------------------------.
;; | The sixteenth commandment                                                  |
;; |                                                                            |
;; | Use (set! ...) only with names defined in (let ...)s.                      |
;; |                                                                            |
;; '----------------------------------------------------------------------------'

;; function with the same definition, which doesn't share the local variable `x'
(define gobbler
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x '())))))

(gobbler 'gumbo)

;; .----------------------------------------------------------------------------.
;; | The seventeenth commandment (preliminary version)                          |
;; |                                                                            |
;; | Use (set! ...) for (let ((x ...)) ...) only if there is at least one       |
;; | (lambda ... between it and the (let ((x ...)) ...).                        |
;; '----------------------------------------------------------------------------'

(define food 'none)
(define (glutton x)
  (set! food x)  ;; it violates 17th commandment by intention
  (cons 'more
        (cons x (cons 'more (cons x '())))))

(glutton 'onion)
food

;; A naive version to swap `x' and `food'
(define chez-nous
  (lambda ()
    (set! x food)
    (set! food x)))

x
food
(chez-nous) ;; which fails the swap

;; .----------------------------------------------------------------------------.
;; | The eighteenth commandment                                                 |
;; |                                                                            |
;; | Use (set! x ...) only when the value that x refers to is no longer needed. |
;; '----------------------------------------------------------------------------'

;; A corrected version
(define chez-nous
  ((let ((y x))
     (lambda ()
       (set! x food)
       (set! food y)))))

(glutton 'garlic)
(gourmand 'potato)
(cons food x)
(chez-nous)
(cons food x)
