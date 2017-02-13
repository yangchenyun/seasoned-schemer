(define (find n Ns Rs)
  (letrec ((A (lambda (ns rs)
                (cond
                 ((null? rs) #f)
                 ((eq? n (car ns)) (car rs))
                 (else (A (cdr ns) (cdr rs)))))))
    (A Ns Rs)))

(define (eqlist? l1 l2)
  (cond
   ((and (null? l1) (null? l2)) #t)
   ((null? l1) #f)
   ((null? l2) #f)
   ((and (atom? (car l1)) (atom? (car l2)))
    (and (eq? (car l2) (car l2))
         (eqlist? (cdr l1) (cdr l2))))
   (else #f)))

;; the old `deep' defined with `if'
(define (deep m)
  (if (zero? m)
      'pizza
      (cons (deep (sub1 m)) '())))

;; Apply the commandment to protect local procedure and the performance cache we
;; have done in Chapter 16
(define deepM
  (let ((Ns '())
        (Rs '()))
    (letrec ((D
              (lambda (m)
                (if (zero? m)
                    'pizza
                    (cons (deepM (sub1 m)) '())))))
      (lambda (n)
        (let ((exists (find n Ns Rs))))
        (if (atom? exists)
            (let ((N (D n)))
              (set! Rs (cons N Rs))
              (set! Ns (cons n Ns))
              N)
            exists)))))

;; Let's refactor our procedure:

;; As `D' is not used in its definition expression...
(define deepM
  (let ((Ns '())
        (Rs '())
        (D (lambda (m)
             (if (zero? m)
                 'pizza
                 (cons (deepM (sub1 m)) '())))))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((N (D n)))
              (set! Rs (cons N Rs))
              (set! Ns (cons n Ns))
              N)
            exists)))))

;; `D' is only used in one place...
(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((N ((lambda (m)
                        (if (zero? m)
                            'pizza
                            (cons (deepM (sub1 m)) '())))
                      n)))
              (set! Rs (cons N Rs))
              (set! Ns (cons n Ns))
              N)
            exists)))))

;; Apply `(lambda ...)' is equivalent to `let'
(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((N (let ((m n))
                       (if (zero? m)
                           'pizza
                           (cons (deepM (sub1 m)) '())))))
              (set! Rs (cons N Rs))
              (set! Ns (cons n Ns))
              N)
            exists)))))

;; Apply `(lambda ...)' is equivalent to `let'
(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((N (if (zero? n)
                         'pizza
                         (cons (deepM (sub1 n)) '()))))
              (set! Rs (cons N Rs))
              (set! Ns (cons n Ns))
              N)
            exists)))))

;; Let's count how many `cons' are needed for `deep' and `deepM'

(define consC
  (let ((N 0))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

;; But `N' is private in the closure
(define counter #f)
(define consC
  (let ((N 0))
    (set! counter (lambda () N)) ;; Expose the N
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(define (deep m)
  (if (zero? m)
      'pizza
      (consC (deep (sub1 m)) '())))

(deep 5)
(counter)
(deep 7)
(counter)

;; use a `supercounter' to count factorial application of `deep'
(define (supercounter f)
  (letrec ((S (lambda (n)
                (if (zero? n)
                    (f n)
                    (let ()
                      (f n)
                      (S (sub1 n)))))))
    (S 1000)))

(supercounter deep)
(counter)

;; procedure to reset the counter
(define set-counter)
(define consC
  (let ((N 0))
    (set! counter (lambda () N))
    (set! set-counter
      (lambda (n)
        (set! N n))) ;; Expose set N
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(set-counter 0)
(supercounter deep)
(counter)

;; Let's try on `deepM'
(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((N (if (zero? n)
                         'pizza
                         (consC (deepM (sub1 n)) '()))))
              (set! Rs (cons N Rs))
              (set! Ns (cons n Ns))
              N)
            exists)))))

(set-counter 0)
(supercounter deepM)
(counter)

;; Let try consC on the last version of rember1*
(define (rember1*C a l)
  (letrec ((R (lambda (l oh)
                (cond
                 ((null? l) (oh 'no))
                 ((atom? (car l))
                  (cond
                   ((eq? a (car l)) (cdr l))
                   (else
                    (consC (car l) (R (cdr l) oh)))))
                 (else
                  (let ((new-car
                         (call/cc
                          (lambda (oh) (R (car l) oh)))))
                    (if (atom? new-car)
                        (consC (car l) (R (cdr l) oh))
                        (consC new-car (cdr l)))))))))
    (let ((new-l (call/cc (lambda (oh) (R l oh)))))
      (if (atom? new-l) l new-l))))

(set-counter 0)
(rember1*C 'noodles '((food) more (food)))
(counter)

;; Now try consC on the first version of rember1*
(define (rember1*C2 a l)
  (letrec
      ((R (lambda (l)
            (cond
             ((null? l) '())
             ((atom? (car l))
              (cond
               ((eq? a (car l)) (cdr l))
               (else (consC (car l)
                           (R (cdr l))))))
             (else
              (let ((av (R (car l))))
                (cond ((eqlist? av (car l))
                       (consC (car l)
                             (R (cdr l))))
                      (else (consC av (cdr l))))))))))
    (R l)))

(set-counter 0)
(rember1*C2 'noodles '((food) more (food)))
(counter)
