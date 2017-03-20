;; let's play with the list building primitives
(define kons cons)
(define kdr cdr)
(define kar car)

(define kounter)
(define set-kounter)
(define konsC
  (let ((N 0))
    (set! kounter (lambda () N))
    (set! set-kounter
      (lambda (n)
        (set! N n))) ;; Expose set N
    (lambda (x y)
      (set! N (add1 N))
      (kons x y))))


(define (lots n)
  (if (zero? n) '()
      (kons
       'egg
       (lots (sub1 n)))))

(lots 3)
(lots 5)
(lots 12)

(define (lenkth l)
  (if (null? l)
      0
      (add1 (lenkth (kdr l)))))

(lenkth (lots 3))
(lenkth (lots 5))
(lenkth (lots 15))

;; Let `lots' add eggs at the end
(define (add-at-end l)
  (if (null? (kdr l))
      (konsC
       (kar l)
       (kons 'egg '()))
      (konsC (kar l)
            (add-at-end (kdr l)))))

(add-at-end (lots 3))
(kounter)

;; `add-at-end' is O(n) to the length of the list to built, let try to do better.

(define set-kdr set-cdr!)
(define (add-at-end-too l)
  (letrec ((A (lambda (ls)
                (if (null? (kdr ls))
                    (set-kdr ls (kons 'egg '()))
                    (A (kdr ls))))))
    (A l)
    l))

(set-kounter 0)
(add-at-end-too (lots 3))
(kounter) ;; now, the operation is O(1)


;; Approximate `kons'/`kar'/`kdr' with a procedure
(define kons
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr))))

(define kar
  (lambda (c)
    (c (lambda (a d) a))))  ;; select the 1st atom in pair

(define kdr
  (lambda (c)
    (c (lambda (a d) d))))  ;; select the 2nd atom in pair

;; let's try `bons' which would implement `set-cdr!'
(define bons
  (lambda (kar)
    (let ((kdr '()))  ;; NOTE: Compare with kons, the value could be changed
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr x))
         kar
         kdr)))))

(define kar
  (lambda (c)
    (c (lambda (s a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (s a d) d))))

(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))

;; use `set-kdr' and `bons' to define `kons', which could be interpreted as:
;; create a `bons' with its `kdr' set to some value.
(define kons
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr c d)
      c)))


;; Now we finished our own shadow version of kons/kar/kdr/set-kdr.


;; Inspect the identify of both functions

(define dozen (lots 12)) ;; used 12 kons
(define bakers-dozen
  (add-at-end dozen))  ;; used 13 kons

(define bakers-dozen-too
  (add-at-end-too dozen))  ;; used 1 kons

(lenkth dozen) ;; NOTE: This is 13 because the `baker-dozen' construct a new
               ;; list but `bakers-dozen-too' modifies the original `dozen'.

(define bakers-dozen-again
  (add-at-end dozen))  ;; used 14 kons now
(lenkth bakers-dozen-again)

;; The first 12 kons in `dozen' and `bakers-dozen-too' is the same.

;; One definition of the 'sameness'.
(define (eklist? ls1 ls2)
  (cond
   ((null? ls1) (null? ls2))
   ((null? ls2) #f)
   (else (and (eq? (kar ls1) (kar ls2))
              (eklist? (kdr ls1) (kdr ls2))))))

(eklist? bakers-dozen-too bakers-dozen)

;; NOTE: But from the previous note we know they are different, we must
;; introduce another concept of 'sameness': two `kons' are the same, iif change
;; one also changes another
(define (same? c1 c2)
  (let ((t1 (kdr c1))
        (t2 (kdr c2)))
    (set-kdr c1 1)
    (set-kdr c2 2)
    (let ((v (= (kdr c1) (kdr c2))))
      (set-kdr c1 t1)
      (set-kdr c2 t2)
      v)))

(same? dozen dozen) ;; #t
(same? dozen bakers-dozen)  ;; #f
(same? dozen bakers-dozen-too) ;; #t, NOTE: the definition doesn't concern about the VALUE of within a pair
(same? bakers-dozen-too bakers-dozen) ;; #f, the book is probably wrong

(define (last-kons ls)
  (if (null? (kdr ls))
      ls
      (last-kons (kdr ls))))
(define long (lots 12))
(set-kdr (last-kons long) long)  ;; This create a cycle

;; (set-kdr (last-kons long) (kdr (kdr long)))  ;; infinite loop with a cycle,
;; (lenkth (set-kdr (last-kons long) long))  ;; will be an infinite loop now

;; A non-functional procedure may use a hash table to store 'seen' node and
;; detect the cycle. How to do that in a functional way - without using a state?

;; egg->egg->egg->egg
;;       ^         |
;;       |---------<

;; The idea is to use Floyd's Tortoise and hare algorithm: keep two pointers in
;; the list.

;; TODO: An interesting problem, how to implement the Floyd's algorithm in a
;; functional way to return mu + lambda.

(define (finite-lenkth l)
  (call/cc
   (lambda (infinite)
     (letrec ((L (lambda (p q)
                (cond
                 ((same? p q) (infinite #f))
                 ;; because 'hare' forwards by 2, there are two terminal cases
                 ((null? q) 0)
                 ((null? (kdr q)) 1)
                 (else
                  ;; use the 'hare' pointer for natural recursion
                  (+ 2 (L (kdr p) (kdr (kdr q)))))))))
       (if (null? l) 0
           (add1 (L l (kdr l))))))))

(finite-lenkth (lots 12))
(finite-lenkth long)

; Guy's Favorite Pie

;; "a la mode" means "served with ice cream". so the joke is here is having a pie
;; and ice cream after ice cream

(define mongo
  (kons (quote pie)
        (kons (quote a)
              (kons (quote la)
                    (kons (quote mode) (quote()))))))
(set-kdr (kdr (kdr (kdr mongo))) (kdr mongo))
