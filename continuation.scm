;; Try to understand continuation as a language primitive.

;; We can define `any desired sequential control abstraction' (e.g., iteration,
;; conditionals, repetition, co-routines, threads, lazy-evaluation, gotos) using
;; first-class continuations ([OCWC]). The corollary of this is that
;; continuations are yet another primitive, such as lambda, from which to build
;; language features or, in other words, new (specialized) languages, from which
;; to solve the particular computing problem at hand.

;; D.P. Friedman and M. Felleisen. The Seasoned Schemer. MIT Press, Cambridge, MA, 1996.
;; [OCWC]	C.T. Haynes, D.P. Friedman and M. Wand. Obtaining Coroutines With Continuations. Computer Languages, 11(3/4), 143-153, 1986.

;; More Resources
;; http://community.schemewiki.org/?call-with-current-continuation
;; https://www.wikiwand.com/en/Call-with-current-continuation
;; http://lambda-the-ultimate.org/node/86
;; http://archive.is/EHKT

;; http://matt.might.net/articles/by-example-continuation-passing-style/
;; http://matt.might.net/articles/programming-with-continuations--exceptions-backtracking-search-threads-generators-coroutines/

;; Related topics for ES6 generator
;; https://gist.github.com/Benvie/5667557

;; NOTE:

;; Concretely, a *continuation* is a procedure that represents the remaining
;; steps in a computation.

;; `call/cc' takes a procedure and passes it the *current continuation*. If
;; invoked, *current-continuation* immediately returns from the call to
;; `call/cc', and `call/cc' returns whatever value was passed to
;; current-continuation.

(define (current-continuation)
  (call/cc (lambda (cc) (cc cc))))


;; ???: Explain the procedures.
(define cc (current-continuation))
(cc 1)
cc ;; 1


;; ???: Explain the procedures.
(define (right-now)
  (call/cc (lambda (cc) (cc cc))))

;; returns the computation to a previously captured moment.
(define (go-when then)
  (then then))

(let ((the-beginning (right-now)))
  (display "Hello, world!")
  (newline)
  (go-when the-beginning))



;; DONE: Explain the procedures.
;;; Return the first element in LST for which WANTED? returns a true
;;; value.
(define (search wanted? lst)
  (call/cc
   (lambda (return)
     (for-each (lambda (element)
                 (if (wanted? element)
                     (return element)))
               lst)
     #f)))

;; The call to `call/cc' returns as soon as `return' (the continuation)
;; procedure is called.

;; procedures don't return to the place they are called; procedures could be
;; thought in CPS style.


;; DONE: Explain the procedures.
(define return #f)

(+ 1 (call/cc
      (lambda (cont)
        (set! return cont)
        1)))

(return 22)  ;; => 23

;; 22 is used as return value of `call/cc', and this value is used to evaluate (+ 1 22)


;; NOTE: It looks as if, the procedure is paused at `call/cc'; and this paused
;; procedure captures its environment in a closure. When the continuation
;; procedure is called, the procedure resumes and returns its value as the
;; application of continuation.


;; ??? Explain the procedures.
(define (hefty-computation do-other-stuff)
  (let loop ((n 5))
    (display "Hefty computation: ")
    (display n)
    (newline)
    (set! do-other-stuff (call/cc do-other-stuff))
    (display "Hefty computation (b)")
    (newline)
    (set! do-other-stuff (call/cc do-other-stuff))
    (display "Hefty computation (c)")
    (newline)
    (set! do-other-stuff (call/cc do-other-stuff))
    (if (> n 0)
        (loop (- n 1)))))

;; notionally displays a clock
(define (superfluous-computation do-other-stuff)
  (let loop ()
    (for-each (lambda (graphic)
                (display graphic)
                (newline)
                (set! do-other-stuff (call/cc do-other-stuff)))
              '("Straight up." "Quarter after." "Half past."  "Quarter til."))
    (loop)))

(hefty-computation superfluous-computation)


;; ??? Explain the procedures
(define (generate-one-element-at-a-time lst)

  ;; Hand the next item from a-list to "return" or an end-of-list marker
  (define (control-state return)
    (for-each
     (lambda (element)
       (set! return (call/cc ;; ??? what does the `set!' do? Why it passes control.
                     (lambda (resume-here)
                       ;; Grab the current continuation
                       (set! control-state resume-here)
                       (return element)))))
     lst)
    (return 'you-fell-off-the-end))

  ;; (-> X u 'you-fell-off-the-end)
  ;; This is the actual generator, producing one item from a-list at a time
  (define (generator) (call/cc control-state))

  ;; Return the generator
  generator)

(define generate-digit
  (generate-one-element-at-a-time '(0 1 2)))

(generate-digit) ;; 0
(generate-digit) ;; 1
(generate-digit) ;; 2
(generate-digit) ;; you-fell-off-the-end


;; ??? Explain the procedures: breaking the Perl lock on cryptic .sigs:
(list->string
 (call/cc
  (lambda(x)
    (let* ((z (map(lambda (c)
                    (reverse
                     (cons c (call/cc (lambda(k) (list k 0))))))
                  (string->list "patsonm")))
           (i (apply max(map car z)))
           (cs (cddr(assoc i z)))
           (xs (append(string->list
                       (case i
                         ((4)"ul")
                         ((5)"@")
                         ((6)"i")
                         ((12)"c.")
                         ((15)(x cs))
                         (else "")))
                      cs))
           (k (cadr
               (list-ref
                (map (lambda(i)(list-ref z i))
                     (list 3 0 4 1 2 5 4 4 5 2 3 5 4 1 6))i))))
      (k `(,@xs ,k ,(+ i 1)))))))


;; ??? Explain the procedures
(let* ((yin
        ((lambda (cc) (display #\@) cc) (call/cc (lambda (c) c))))
       (yang
        ((lambda (cc) (display #\*) cc) (call/cc (lambda (c) c)))))
  (yin yang))
