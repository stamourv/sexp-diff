#lang scribble/doc

@(require planet/scribble planet/version scribble/eval scribble/manual)

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval `(require (planet ,(this-package-version-symbol))))
    the-eval))

@title[#:tag "top"]{sexp-diff}
@author+email["Vincent St-Amour" "stamourv@ccs.neu.edu"]

This package provides an S-expression-aware diffing tool based on
Levenshtein-like tree edit distance.

@defproc[(sexp-diff [e1 sexp?] [e2 sexp?])
         sexp?]{
 Produces a tree that corresponds to the common structure of @racket[e1] and
 @racket[e2], with @racket[e1]-specific parts tagged with @racket[#:old] and
 @racket[e2]-specific parts tagged with @racket[#:new].
            
 @examples[#:eval the-eval
          (sexp-diff
           '(define (f x) (+ (* x 2) 1)) 
           '(define (f x) (- (* x 2) 3 1)))
          (sexp-diff
           '(define (f x) (+ (* x 2) 4 1))
           '(define (f x) (- (* x 2) 5 3 1)))
          (sexp-diff
           '(define (f x) (+ (* x 2) 4 4 1))
           '(define (f x) (- (* x 2) 5 5 3 1)))
	   ]
}
