;; the definition of `multirember' from chapter 3
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a)
      (multirember a (cdr lat)))
     (else
      (cons (car lat) (multirember a (cdr lat)))))))

;; Let's try omit `a' from the natural recursion
(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
             ((null? lat) '())
             ((eq? a (car lat)) (mr (cdr lat)))
             (else (cons (car lat)
                         (mr (cdr lat)))))
            )))
     lat)))

;; Let's discover what Y actually is
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;; redefine `length' using Y
(define length
  (Y (lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))))

;; let's rewrite the original `multirember' with a bounded fn

(define multirember
  (lambda (a lat)
    ((letrec ((mr (lambda (lat)
                   (cond
                    ((null? lat) '())
                    ((eq? (car lat) a)
                     (multirember a (cdr lat)))
                    (else
                     (cons (car lat) (multirember a (cdr lat))))))))
       mr) ;; `mr' has `a' bound
     lat)))  ;; apply `mr' with `lat'.

(define multirember
  (lambda (a lat)
    (letrec ((mr (lambda (lat)
                   (cond
                    ((null? lat) '())
                    ((eq? (car lat) a)
                     (multirember a (cdr lat)))
                    (else
                     (cons (car lat) (multirember a (cdr lat))))))))
      (mr lat)))) ;; the same definition but apply `mr' to `lat' immediately.

;; .----------------------------------------------------------------------------.
;; | The twelfth commandment                                                    |
;; |                                                                            |
;; | Use (letrec ...) to remove arguments that do not change for recursive      |
;; | applications.                                                              |
;; '----------------------------------------------------------------------------'

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) a)
        ((multirember-f test?) a (cdr lat)))
       (else
        (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

;; Obviously, `test?' is always the same during the natural recursion, so let's apply the 12th commandment

(define multirember-f
  (lambda (test?)
    (letrec ((mf (lambda (a lat)
               (cond
                ((null? lat) '())
                ((test? (car lat) a)
                 ((multirember-f test?) a (cdr lat)))
                (else
                 (cons (car lat) ((multirember-f test?) a (cdr lat))))))))
      mf)))

;; redefine `multirember' with `multirember-f'
(multirember-f eq?)

(define multirember
  (letrec ((mf (lambda (a lat)
                 (cond
                  ((null? lat) '())
                  ((eq? (car lat) a)
                   (multirember a (cdr lat)))
                  (else
                   (cons (car lat) (multirember a (cdr lat))))))))
    mf))

;; Also rename `mf' as `multirember' and we have another definition
(define multirember
  (letrec ((multirember (lambda (a lat)
                 (cond
                  ((null? lat) '())
                  ((eq? (car lat) a)
                   (multirember a (cdr lat)))
                  (else
                   (cons (car lat) (multirember a (cdr lat))))))))
    multirember))

;; Eliminate `letrec' in the above because there is no local binding
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a)
      (multirember a (cdr lat)))
     (else
      (cons (car lat) (multirember a (cdr lat)))))))

;; the original definition of `member?' from ch2
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     ((eq? (car lat) a) #t)
     (else (member? a (cdr lat))))))

;; Also apply 12th commandment
(define member?
  (lambda (a lat)
    (letrec ((m (lambda (lat)
                  (cond
                   ((null? lat) #f)
                   ((eq? (car lat) a) #t)
                   (else (m (cdr lat)))))
                ))
      (m lat))))

;; The original definition of `union'
(define union
  (lambda (s1 s2)
    (cond
     ((null? s1) s2)
     ((member? (car s1) s2)
      (union (cdr s1) s2))
     (else (cons (car s1) (union (cdr s1) s2))))))

;; Apply 12th commandment again
(define union
  (lambda (s1 s2)
    (letrec
        ((U (lambda (set)
              (lambda (set s2)
                (cond
                 ((null? set) s2)
                 ((member? (car set) s2)
                  (U (cdr set)))
                 (else (cons (car set) (U (cdr set)))))))))
      (U s1))))

;; still curious for a better name of U

;; Also define `member?' inside `letrec' in order to hide this helper function
(define union
  (lambda (s1 s2)
    (letrec
        ((U (lambda (set)
              (lambda (set s2)
                (cond
                 ((null? set) s2)
                 ((M? (car set) s2)
                  (U (cdr set)))
                 (else (cons (car set) (U (cdr set))))))))
         (M? (lambda (a lat)
               (cond
                ((null? lat) #f)
                ((eq? (car lat) a) #t)
                (else (M? a (cdr lat)))))))
      (U s1))))

;; .----------------------------------------------------------------------------.
;; | The thirteenth commandment                                                 |
;; |                                                                            |
;; | Use (letrec ...) to hide and to protect functions.                         |
;; '----------------------------------------------------------------------------'

;; The `M?' violates the 12th commandment,
(define union
  (lambda (s1 s2)
    (letrec
        ((U (lambda (set)
              (lambda (set s2)
                (cond
                 ((null? set) s2)
                 ((M? (car set) s2)
                  (U (cdr set)))
                 (else (cons (car set) (U (cdr set))))))))
         (M? (lambda (a lat)
               (letrec
                   ((N? (lambda lat)
                        (cond
                         ((null? lat) #f)
                         ((eq? (car lat) a) #t)
                         (else (N? (cdr lat)))))))
               (N? lat))))
      (U s1))))

;; Apply 13th commandment to `two-in-a-row?'

;; The lexical definition turns out to be:
(define two-in-a-row?
  (lambda (lat)
    (letrec ((W (lambda (proceeding lat)
                  (cond
                   ((null? lat) #f)
                   (else
                    (or (eq? proceeding (car lat))
                        (W (car lat) (cdr lat)))))))))
    (cond
     ((null? lat) #f)
     (else (W (car lat) (cdr lat))))))

;; `W' doesn't need to know about `lat'
(define two-in-a-row?
  (letrec ((W (lambda (proceeding lat)
                (cond
                 ((null? lat) #f)
                 (else
                  (or (eq? proceeding (car lat))
                      (W (car lat) (cdr lat))))))))
    (lambda (lat)
      (cond
       ((null? lat) #f)
       (else (W (car lat) (cdr lat)))))))

;; Also protect `sum-of-prefixes' with 13th commandment
(define sum-of-prefixes
  (letrec ((S (lambda (sonssf tup) ;; sonssf: Sum Of Numbers Seen So Far
                (cond
                 ((null? tup) '())
                 (else
                  (cons (+ sonssf (car tup))
                        (S (+ sonssf (car tup)) (cdr tup))))))))
    (lambda (tup) (S 0 tup))))

;; `scramble' too
(define scramble
  (letrec ((P (lambda (rp tup)
                (cond
                 ((null? tup) '())
                 (else
                  (cons (nth (car tup) (cons (car tup) rp))
                        (P (cons (car tup) rp) (cdr tup)))))))
           (nth (lambda (n lat)
                  (cond
                   ((eq? n 1) (car lat))
                   (else (nth (sub1 n) (cdr lat)))))))
    (lambda (tup) (P '() tup))))

;; tests to verify it works too
(scramble '(1 1 1 3 4 2 1 1 9 2))
(scramble '(1 2 3 4 5 6 7 8 9))
(scramble '(1 2 3 1 2 3 4 1 8 2 10))
