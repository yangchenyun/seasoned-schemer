;; `atom?' which identifies '() as #f
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; `leftmost' from chapter 5
(define (leftmost l)
  (cond
   ((atom? (car l)) (car l))
   (else (leftmost (car l)))))

(leftmost '(((a) b) (c d)))
(leftmost '(((a) ()) () (e)))
(leftmost '(((()) a) ()))               ;; `leftmost' fails to return `a' in this case.

;; `leftmost' which handles '()
(define (leftmost l)
  (cond
   ((null? l) '())
   ((atom? (car l)) (car l))
   (else
    (cond ((atom? (leftmost (car l)))
           (leftmost (car l)))
          (else (leftmost (cdr l)))))))

(leftmost '(((()) a) ()))

;; using `let' to remove repeated expression
(define (leftmost l)
  (cond
   ((null? l) '())
   ((atom? (car l)) (car l))
   (else
    (let ((a (leftmost (car l))))
      (cond ((atom? a) a)
            (else (leftmost (cdr l))))))))

(define (eqlist? l1 l2)
  (cond
   ((and (null? l1) (null? l2)) #t)
   ((null? l1) #f)
   ((null? l2) #f)
   ((and (atom? (car l1)) (atom? (car l2)))
    (and (eq? (car l2) (car l2))
         (eqlist? (cdr l1) (cdr l2))))
   (else #f)))

;; `rember1*'
(define (rember1* a l)
  (cond
   ((null? l) '())
   ((atom? (car l))
    (cond
     ((eq? a (car l)) (cdr l))
     (else (cons (car l)
                 (rember1* a (cdr l))))))
   (else
    (cond ((eqlist?
            (rember1* a (car l))
            (car l))
           (cons (car l)
                 (rember1* a (cdr l))))
          (else (cons (rember1* a (car l))
                      (cdr l)))))))

(rember1* 'meat '((pasta meat)
                  pasta
                  (noodles meat sauce)
                  meat tomatoes))

;; Apply 12th commandment and `let` to `rember1*'
(define (rember1* a l)
  (letrec
      ((R (lambda (l)
            (cond
             ((null? l) '())
             ((atom? (car l))
              (cond
               ((eq? a (car l)) (cdr l))
               (else (cons (car l)
                           (R (cdr l))))))
             (else
              (let ((av (R (car l))))
                (cond ((eqlist? av (car l))
                       (cons (car l)
                             (R (cdr l))))
                      (else (cons av (cdr l))))))))))
    (R l)))

;; .----------------------------------------------------------------------------.
;; | The fifteenth commandment (preliminary version)                            |
;; |                                                                            |
;; | Use (let ...) to name the values of repeated expressions.                  |
;; '----------------------------------------------------------------------------'

;; `depth*' calculates the innermost nested level on a list of S-expression
(define (depth* l)
  (let ((dr (depth* (cdr l))))
    (cond
     ((null? l) 1)
     ((atom? (car l)) dr)
     (else
      (let ((da (depth* (car l))))
        (cond
         ((> dr (add1 da)) dr)
         (else (add1 da))))))))

(depth* '(a (b (c d) a) a))
(depth* '(()  a (b (c d) a) a))         ;; also works when (car l) is '()
(depth* '())                            ;; this is a BUG

(define (depth* l)
  (cond
   ((null? l) 1)
   (else
    (let ((dr (depth* (cdr l))))
      (cond
       ((null? l) 1)
       ((atom? (car l)) dr)
       (else
        (let ((da (depth* (car l))))
          (cond
           ((> dr (add1 da)) dr)
           (else (add1 da))))))))))

(depth* '())                            ;; this works now, but ...

;; The final version from textbook, some missed points:
;; - declares a binding for (add1 ...)
;; - avoid introduce the `dr' binding, as it doesn't save evaluation and makes
;; the program harder to read

(define (depth* l)
  (cond
   ((null? l) 1)
   ((atom? (car l)) (depth* (cdr l)))
   (else
    (let ((a (add1 (depth* (car l))))
          (d (depth* (cdr l))))
      (cond
       ((> d a) d)
       (else a))))))


;; .----------------------------------------------------------------------------.
;; | The fifteenth commandment (revised version)                                |
;; |                                                                            |
;; | Use (let ...) to name the values of repeated expressions in a function     |
;; | definition if they may be evaluated twice for one and the same use of the  |
;; | function.                                                                  |
;; '----------------------------------------------------------------------------'

;; Make `depth*' more enjoyable with `if' form and
(define (max m n)
  (if (> n m) n m))

(define (depth* l)
  (cond
   ((null? l) 1)
   ((atom? (car l)) (depth* (cdr l)))
   (else
    (max
     (add1 (depth* (car l)))
     (depth* (cdr l))))))


;; `let'ting the `scramble'
(define scramble
  (letrec ((P (lambda (rp tup)
                (cond
                 ((null? tup) '())
                 (else
                  (let ((rp (cons (car tup) rp)))
                    (cons (nth (car tup) rp)
                          (P rp (cdr tup))))))))
           (nth (lambda (n lat)
                  (cond
                   ((eq? n 1) (car lat))
                   (else (nth (sub1 n) (cdr lat)))))))
    (lambda (tup) (P '() tup))))


;; use continuation to return the value once we found it
(define (leftmost l)
  (call/cc
   (lambda (ret)
     (cond
      ((null? l) '())
      ((atom? (car l)) (ret (car l)))
      (else
       (let ((a (leftmost (car l))))
         (cond ((atom? a) a)
               (else (leftmost (cdr l))))))))))

;; Version from the text:
;; - the condition check is removed
;; - applied 12th Commandment for `skip'

(define (leftmost l)
  (call/cc
   (lambda (skip)
     (letrec ((lm (lambda (l)
                   (cond
                    ((null? l) '())
                    ((atom? (car l)) (skip (car l)))
                    (else (begin ;; the condition check is unnecessary as
                            ;; `lm' will return from continuation
                            ;; argument, and the stack here is never
                            ;; reached.
                            (lm (car l))
                            (lm (cdr l))))))))
       (lm l)))))

;; The definition also becomes very clear:
;; `leftmost' checks atoms from left to right until it finds one and returns.


;; Now let's simplify `rember1*' with continuation as well

(define (rember1* a l)
  (letrec ((R (lambda (l)
                (cond
                 ((null? l) '())
                 ((atom? (car l))
                  (cond
                   ((eq? a (car l)) (cdr l))
                   (else
                    (cons (car l) (R (cdr l))))))
                 (else
                  (let (av (R (car l)))
                    (cond
                     ((eqlist? av (car l))
                      (cons (car l) (R (cdr l)))) ;; When no a is removed, continue the procedure
                     (else
                      (cons av (cdr l))))))))))
    (R l)))

;; The idea is to signal when `R' didn't remove the `a' in `(R (car l))'

(define (rember1* a l)
  (letrec ((R (lambda (l oh)
                (cond
                 ((null? l) (oh 'no))
                 ((atom? (car l))
                  (cond
                   ((eq? a (car l)) (cdr l))
                   (else
                    (cons (car l) (R (cdr l) oh)))))
                 (else
                  (if (atom?
                       (call/cc
                        (lambda (oh) (R (car l) oh))))
                      (cons (car l) (R (cdr l) oh))
                      (cons (R (car l) 0) (cdr l)))))))) ;; 0 is a placeholder argument
    (if (atom?
         (call/cc (lambda (oh) (R l oh))))
        l                               ;; handle the case when the remove failed for entire list
        (R l '()))))                    ;; '() is a placeholder argument

(rember1* 'noodle '((food) meat (noodle food)))

;; Apply 15th commandment
(define (rember1* a l)
  (letrec ((R (lambda (l oh)
                (cond
                 ((null? l) (oh 'no))
                 ((atom? (car l))
                  (cond
                   ((eq? a (car l)) (cdr l))
                   (else
                    (cons (car l) (R (cdr l) oh)))))
                 (else
                  (let ((new-car
                         (call/cc
                          (lambda (oh) (R (car l) oh)))))
                    (if (atom? new-car)
                        (cons (car l) (R (cdr l) oh))
                        (cons new-car (cdr l)))))))))
    (let ((new-l (call/cc (lambda (oh) (R l oh)))))
      (if (atom? new-l) l new-l))))


;; Remove the condition checking form with continuation
;;
;; ???: Under which situation will 'choice-B be returned?
;; 
;; (call/cc
;;  (lambda (success)
;;    (call/cc
;;     (lambda (oh) (success 'choice-A)))
;;    'choice-B))

(define (rember1* a l)
  (letrec ((R (lambda (l oh)
                (cond
                 ((null? l) (oh 'no))
                 ((atom? (car l))
                  (cond
                   ((eq? a (car l)) (cdr l))
                   (else
                    (cons (car l) (R (cdr l) oh)))))
                 (else
                  (call/cc
                   (lambda (success)
                     (call/cc
                      (lambda (oh)
                        (success (cons (R (car l) oh) (cdr l)))))
                     (cons (car l) (R (cdr l) oh)))))))))
    (call/cc
     (lambda (success)
       (call/cc
        (lambda (oh)
          (success (R l oh))))
       l))))

(rember1* 'noodle '((food) meat (noodle food)))
