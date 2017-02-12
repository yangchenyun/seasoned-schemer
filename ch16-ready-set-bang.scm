(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (letrec ((m (lambda (lat)
                  (cond
                   ((null? lat) #f)
                   ((eq? (car lat) a) #t)
                   (else (m (cdr lat)))))
                ))
      (m lat))))

(define (sweet-tooth food)
  (cons food (cons 'cake '())))
(define last 'anglefood)

(sweet-tooth last)
(let ((x 'fruit))
  (sweet-tooth x))

;; Adding side effect of an assignment
(define (sweet-toothL food)
  (set! last food)
  (cons food (cons 'cake '())))

(sweet-toothL 'chocolate)
last
(sweet-toothL 'fruit)
last
(sweet-toothL 'cheese)
(sweet-toothL 'carrot)

(define ingredients '())

;; Also remember the food eaten before
(define (sweet-toothR food)
  (set! ingredients (cons food ingredients))
  (cons food (cons 'cake '())))

(sweet-toothR 'chocolate)
ingredients
(sweet-toothR 'fruit)
ingredients
(sweet-toothR 'cheese)
ingredients
(sweet-toothR 'carrot)
ingredients


(define (deep m)
  (cond
   ((zero? m) 'pizza)
   (else (cons (deep (sub1 m)) '()))))

(deep 3)
(deep 7)
(deep 0)

;; Also remember the arguments pass to `deep'
(define Ns '())
(define (deepR n)
  (set! Ns (cons n Ns))
  (deep n))
(deepR 3)
(deepR 6)
Ns

;; Remember the results pass to `deep' too
(define Rs '())
(define (deepR n)
  (let ((N (deep n)))
    (set! Rs (cons N Rs))
    (set! Ns (cons n Ns))
    N))

(deepR 3)
(deepR 6)
(deepR 5)
Ns
Rs

;; .----------------------------------------------------------------------------.
;; | The nineteenth commandment                                                 |
;; |                                                                            |
;; | Use (set! ...) to remember valuable things between two distinct uses of a  |
;; | function.                                                                  |
;; '----------------------------------------------------------------------------'

;; An observation that, Rs could be used to as answer to `deepR' if the result
;; has been calculated before.

;; Function which finds value in Rs if the argument has appeared in Ns.

(define (find n Ns Rs)
  (cond
   ((null? Rs) #f)
   ((eq? n (car Ns)) (car Rs))
   (else (find n (cdr Ns) (cdr Rs)))))

(find 5 Ns Rs)

;; Apply 12th commandment to remove `n'
(define (find n Ns Rs)
  (letrec ((A (lambda (ns rs)
                (cond
                 ((null? rs) #f)
                 ((eq? n (car ns)) (car rs))
                 (else (A (cdr ns) (cdr rs)))))))
    (A Ns Rs)))

;; Now use `find' with `deep'
(define (deepM n)
  (if (member? n Ns)
      (find n Ns Rs)
      (let ((N (deep n)))
        (set! Rs (cons N Rs))
        (set! Ns (cons n Ns))
        N)))

;; now the result of `deepM' is cached, but `deep' doesn't

(define (deep m)
  (cond
   ((zero? m) 'pizza)
   (else (cons (deepM (sub1 m)) '()))))

(deepM 9)
Ns

;; Now apply 17th commandment to `deepM'
(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (if (member? n Ns)
          (find n Ns Rs)
          (let ((N (deep n)))
            (set! Rs (cons N Rs))
            (set! Ns (cons n Ns))
            N)))))

(deepM 12)
Ns

;; replace the responsibility of `member?' with `find' returns #f
(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (let ((exists (find n Ns Rs))))
      (if (atom? exists)
          (let ((N (deep n)))
            (set! Rs (cons N Rs))
            (set! Ns (cons n Ns))
            N)
          exists))))

(deepM 20)

;; New `length' definition using !set
(define length
  (lambda (l) 0))

(set! length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (length (cdr l)))))))

;; apply 17th commandment
(define length
  (let ((h (lambda (l) 0)))
    (set! h
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (h (cdr l)))))))
    h))

(length '())
(length '(a b x))

;; .----------------------------------------------------------------------------.
;; | The seventeenth commandment (final version)                                |
;; |                                                                            |
;; | Use (set! x ...) for (let ((x ...)) ...) only if there is at least one     |
;; | (lambda ... between it and the (let ...), or if the new value for x is a   |
;; | function that refers to x                                                  |
;; '----------------------------------------------------------------------------'

;; Step by step walking through of the new version of `length'

;; The above definition is equivalent to..
(define h1 (lambda (l) 0))
(define length
  (let ()
    (set! h1
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (h1 (cdr l)))))))
    h))

;; It is equivalent to..
(define h1
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (h1 (cdr l)))))))
(define length
  (let ()
    h1))

;; It is equivalent to ..
(define length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (h1 (cdr l)))))))

;; Now `length' refers to a recursive copy through the local binding `h'


;; Try to eliminate the parts of definition specific to length
(define L
  (lambda (h)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (h (cdr l))))))))

(define length
  (let ((h (lambda (l) 0)))
    (set! h (L h)) ;; This is WRONG, `h' here will always be `lambda (l) 0'
    h))

(length '()) ;;  0
(length '(1 2 3)) ;; => 1, WRONG

;; Wrap h in an anonymous function
(define length
  (let ((h (lambda (l) 0)))
    (set! h (L (lambda (args) (h args))))
    h))

(define length
  (let ((h (lambda (l) 0)))
    (set! h (L (lambda (args) (h args))))
    h))

;; the value of `h' is:
(L (lambda (args) (h args)))

;; applying `L', and this is the value of `h' itself. It is exactly recursive
;; definition of `length'
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 ((lambda (args) (h args)) ;; `h' refers to itself here.
                (cdr l))))))

(length '(1 2 3)) ;; => 3, Correct

;; Interestingly, the initial value of `h' is irrelevant and is never used.

;; So rewrite length as a function of L
(define Y-bang
  (lambda (L)
    (let ((h 'anything))
      (set! h
        (L (lambda (args) (h args))))
      h)))

;; Or using abbreviation of `letrec' for recursive definition
(define Y-bang
  (lambda (f)
    (letrec
        ((h (f (lambda (args) (h args)))))
      h)))

(define length
  (Y-bang L))

;; Let's try redefining depth*
(define (max m n)
  (if (> n m) n m))

;; The original definition
(define (depth* l)
  (cond
   ((null? l) 1)
   ((atom? (car l)) (depth* (cdr l)))
   (else
    (max
     (add1 (depth* (car l)))
     (depth* (cdr l))))))

(depth* '(a (((b (c (d)))))))           ;; => 6

(define D
  (lambda (depth*) ;; `depth*' is not defined in the environment at all
    (lambda (l)
      (cond
       ((null? l) 1)
       ((atom? (car l)) (depth* (cdr l)))
       (else
        (max
         (add1 (depth* (car l)))
         (depth* (cdr l))))))))

(define depth* (Y-bang D))
(depth* '(a (((b (c (d)))))))           ;; => 6, still works!


;; Recall `Y' in chapter 12
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;; Both Y-bang and Y produces the same recursive function for `f'

;; Let's try something bizarre

(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (add1 x)) ;; a closure state
      (lambda (a)
        (if (= a x)
            0
            (f a))))))

((Y biz) 5)
((Y-bang biz) 5) ;; result in infinite recursion
