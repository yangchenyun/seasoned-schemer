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

;; NOTE: Definition of a 'tup': a list of numbers.
;; guess the definition of `scramble':

(1 1 1 3 4 2 1 1 9 2)
;; (1 1 1 1 1 4 1 1 1 9)

(1 2 3 4 5 6 7 8 9)
;; (1 1 1 1 1 1 1 1 1)

(1 2 3 1 2 3 4 1 8 2 10)
;; (1 1 1 1 1 1 1 1 2 8 2)

;; My first guess:
;; Consumes a non-empty tup of positive integers and outputs a list of same
;; length. The nth item of the output is determined by:
;;   - if the nth item in origin list is 1, output 1
;;   - if the nth item in origin list is greater than n-1th item, output 1
;;   - if the nth item in origin list is smaller than n-1th item, use the nth
;;     item as index into the last longest sequence form.
(1 1 1 3 4
   2 ;; index 2 in (1 3 4)
   1 1 9
   2 ;; index 2 in (1 9) ??? The definition doesn't work
   )

;; Definition in Book
;; Consumes a non-empty tup where no number is greater than its own index. The
;; nth value is used to as backward index in the prefix sequence (including
;; itself) to get the nth value in output list.

(define nth
  (lambda (n lat)
    (cond
     ((eq? n 1) (car lat))
     (else (nth (sub1 n) (cdr lat))))))

(define scramble-with-reversed-prefix
  (lambda (lat reversed-prefix)
    (cond
     ((null? lat) '())
     (else
      (cons (nth (car lat) (cons (car lat) reversed-prefix))
            (scramble-with-reversed-prefix
             (cdr lat) (cons (car lat) reversed-prefix) ))))))

(define scramble
  (lambda (lat)
    (scramble-with-reversed-prefix lat '())))

;; some tests
(scramble '(1 1 1 3 4 2 1 1 9 2))
(scramble '(1 2 3 4 5 6 7 8 9))
(scramble '(1 2 3 1 2 3 4 1 8 2 10))

;; yeah! it works
