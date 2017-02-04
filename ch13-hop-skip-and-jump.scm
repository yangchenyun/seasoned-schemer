(define member?
  (lambda (a lat)
    (letrec ((m (lambda (lat)
                  (cond
                   ((null? lat) #f)
                   ((eq? (car lat) a) #t)
                   (else (m (cdr lat)))))
                ))
      (m lat))))

(define intersect
  (lambda (s1 s2)
    (cond
     ((null? s1) '())
     ((member? (car s1) s2)
      (cons (car s1) (intersect (cdr s1) s2)))
     (else (intersect (cdr s1) s2)))))

;; Apply 12th commandment
(define intersect
  (lambda (s1 s2)
    (letrec ((I (lambda (s1)
                  (cond
                   ((null? s1) '())
                   ((member? (car s1) s2)
                    (cons (car s1) (I (cdr s1))))
                   (else (I (cdr s1)))))))
      (I s1))))

;; `intersectall' computes the intersect of a list of sets under the assumption
;; each set is non-empty
(define intersectall
  (lambda (lset)
    (cond
     ((null? (cdr lset)) (car lset))
     (else
      (intersect (car lset)
                 (intersectall (cdr lset)))))))

(intersectall '((a b c) (b c) (c)))

;; remove the non-empty assumption
(define intersectall
  (lambda (lset)
    (cond
     ((null? lset) '())
     ((null? (cdr lset)) (car lset))
     (else
      (intersect (car lset)
                 (intersectall (cdr lset)))))))

;; `null? lset' only need to be executed once, so apply 12th commandment
(define intersectall
  (lambda (lset)
    (letrec
        ((A (lambda (lset)
              (cond
               ((null? (cdr lset)) (car lset))
               (else
                (intersect (car lset)
                           (A (cdr lset))))))))
      (cond
       ((null? lset) '())
       (else (A lset))))))

(intersectall '((a b c) () (b c)))

;; `intersectall' will continue the recursion even when it the natural recursion
;; includes '(). Modify the function using `call-with-current-continuation' or
;; the `call/cc' form.

(define intersectall
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop) ;;  (catch 'hop ...)
       (letrec
           ((A (lambda (lset)
                 (cond
                  ((null? (car lset)) (hop '())) ;; (throw 'hop '())
                  ((null? (cdr lset)) (car lset))
                  (else
                   (intersect (car lset)
                              (A (cdr lset))))))))
         (cond
          ((null? lset) '())
          (else (A lset))))))))

(intersectall '((a b c) (b c)))
(intersectall '((a b c) () (b c)))      ;; (hop '()) forgets the previous stacks
                                        ;; and returns early

;; .----------------------------------------------------------------------------.
;; | The fourteenth commandment                                                 |
;; |                                                                            |
;; | Use (letcc ...) to return values abruptly and prompty.                     |
;; '----------------------------------------------------------------------------'

(intersectall '((a b c) (d e f) (a)))

;; Another observation on the improved form is that, when sub recursion returns
;; an '(), the recursion still continues.

;; Improved `intersect', which returns '() when s2 is '()
(define intersect
  (lambda (s1 s2)
    (letrec ((I (lambda (s1)
                  (cond
                   ((null? s1) '())
                   ((member? (car s1) s2)
                    (cons (car s1) (I (cdr s1))))
                   (else (I (cdr s1)))))))
      (cond
       ((null? s2) '())
       (else (I s1))))))

;; `intersectall' could still be improved, when one `intersect' returns '(),
;; `intersectall' could return early as well.
(define intersectall
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                  ((null? (car lset)) (hop '()))
                  ((null? (cdr lset)) (car lset))
                  (else
                   (I (car lset)
                      (A (cdr lset)))))))
            (I (lambda (s1 s2)
                 (letrec ((J (lambda (s1)
                               (cond
                                ((null? s1) '())
                                ((member? (car s1) s2)
                                 (cons (car s1) (J (cdr s1))))
                                (else (J (cdr s1)))))))
                   (cond
                    ((null? s2) (hop '())) ;; also returns early on local function
                    ;; (throw 'hop '())
                    (else (J s1)))))))
         (cond
          ((null? lset) '())
          (else (A lset))))))))


;; Let's work on something like `rember', which removes a from `lat' once.
(define rember
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
               ((null? lat) '())
               ((eq? (car lat) a) (cdr lat))
               (else (cons a (cdr lat)))))))
      (R lat))))

;; `rember-beyond-first' takes in an `a' and `lat'. If `a' appears in `lat', it
;; will returns a list where all atoms in lat beyond `a' is removed.
(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ((B (lambda (lat)
              (cond
               ((null? lat) '())
               ((eq? a (car lat)) '())
               (else
                (cons (car lat) (B (cdr lat))))))))
      (B lat))))

(rember-beyond-first 'roots '(noodles bean-thread roots potatos))

;; `rember-upto-last' takes in an `a' and `lat'. If `a' appears in `lat', it
;; will return a list contains atoms in lat from the last `a' until the end.

(define rember-upto-last
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
               ((null? lat) '())
               ((eq? a (car lat)) (cdr lat)) ;; BUG: this doesn't returns the
                                             ;; atoms after the LAST appearance of `a'
               (else (R (cdr lat)))
               ))))
      (R lat))))

(rember-upto-last 'roots '(noodles bean-thread roots potatos))
(rember-upto-last 'roots '(noodles bean-thread roots potatos roots tomatos)) ;; wrong

;; With 14th commandment, `rember-upto-last' would ignore the computation and
;; restart the process.

(define rember-upto-last
  (lambda (a lat)
    (call/cc
     (lambda (skip)
       (letrec
           ((R (lambda (lat)
                 (cond
                  ((null? lat) '())
                  ((eq? a (car lat))
                   (skip (R (cdr lat)))) ;; restart the process from (cdr lat)
                                         ;; and ignored previous consed values
                  (else
                   (cons (car lat) (R (cdr lat))))
                  ))))
         (R lat))))))


;; The last example with skipping is fascinating, this explains the nature of
;; throw/catch, which discard / jump to function stacks when necessary.
