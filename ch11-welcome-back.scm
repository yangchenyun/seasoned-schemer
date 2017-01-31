(define is-first?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (eq? a (car lat))))))

;; In this version of `two-in-a-row?' the decision to continue recursion is
;; determined by the function itself. A rewritten is done to delegate the decision to `is-first?'
(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else
      (or (is-first? (car lat) (cdr lat))
          (two-in-a-row? (cdr lat)))))))

;; The lexical definition turns out to be:
(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else
      (is-first-b? (car lat) (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else
      (or (eq? a (car lat))
       (two-in-a-row? lat))))))  ;; This is the same definition in `two-in-a-row?'

;; Observe that the application of `two-in-a-row?' will actually recursively
;; call `is-first-b?', so manually apply the function inline.
(define is-first-b?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else
      (or (eq? a (car lat))
          (is-first-b? (car lat) (cdr lat)))))))

;; Renaming the function and the parameter to reflect the meaning of the
;; function and `a'. Now the `two-in-a-row-b?' naturally recursive calls itself.

;; NOTE: more on natural recursion:
;; - http://stackoverflow.com/questions/32260444/what-is-the-definition-of-natural-recursion
;; - A note from Dan himself: http://i.stack.imgur.com/QhBcl.png

;; Per the note; `two-in-a-row-b?' is naturally recursive because the
;; "destructing" of `lat' by `(cdr lat)'.
(define two-in-a-row-b?
  (lambda (proceeding lat)
    (cond
     ((null? lat) #f)
     (else
      (or (eq? proceeding (car lat))
          (two-in-a-row-b? (car lat) (cdr lat)))))))

;; step-by-step evaluation example
(two-in-a-row? '(b d e i i a g))  ;; (null? '(b d e i i a g)) => #f
(two-in-a-row-b? 'b '(d e i i a g))  ;; (null? '(d e i i a g)) => #f
(let ((proceeding 'b)
      (lat '(d e i i a g)))
 (or (eq? proceeding (car lat)) ;; => #f
     (two-in-a-row-b? (car lat) (cdr lat))))
(two-in-a-row-b? 'd '(e i i a g))
(let ((proceeding 'd)
      (lat '(e i i a g)))
  (or (eq? proceeding (car lat)) ;; => #f
      (two-in-a-row-b? (car lat) (cdr lat))))
(two-in-a-row-b? 'e '(i i a g))
(let ((proceeding 'e)
      (lat '(i i a g)))
  (or (eq? proceeding (car lat)) ;; => #f
      (two-in-a-row-b? (car lat) (cdr lat))))
(two-in-a-row-b? 'i '(i a g))
(let ((proceeding 'i)
      (lat '(i a g)))
  (or (eq? proceeding (car lat)) ;; => #t
      (two-in-a-row-b? (car lat) (cdr lat))))


(define sum-of-prefixes
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      ;; ??? The procedures would build the list by ~cons~ the previous sums
      ;; onto the natural recursion.
      (sum-of-prefixes (cdr lat))
      ))))

;; Adapt the techniques used in `two-in-a-row-b?', the natural recursion will
;; take one extra argument.
(define sum-of-prefixes-b
  (lambda (sonssf lat) ;; sonssf: Sum Of Numbers Seen So Far
    (cond
     ((null? lat) '())
     (else
      (cons (+ sonssf (car lat))
            (sum-of-prefixes-b
             (+ sonssf (car lat)) (cdr lat)))))))

(define sum-of-prefixes
  (lambda (lat)
    (sum-of-prefixes-b 0 lat)))


;; .----------------------------------------------------------------------------.
;; | The eleventh commandment:                                                  |
;; |                                                                            |
;; | Use additional arguments when a function needs to know what the other      |
;; | arguments to the function have been like so far.                           |
;; '----------------------------------------------------------------------------'

