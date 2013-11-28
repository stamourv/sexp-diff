#lang scribble/doc

@(require scribble/eval scribble/manual)

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require sexp-diff))
    the-eval))

@begin[(require (for-label sexp-diff))]
@(declare-exporting sexp-diff)

@title[#:tag "top"]{sexp-diff}
@author+email["Vincent St-Amour" "stamourv@racket-lang.org"]

This package provides an S-expression-aware diffing tool based on
Levenshtein-like tree edit distance.

@defproc[(sexp-diff [e1 sexp?] [e2 sexp?]
                    [#:old-marker old-marker any/c '#:old]
                    [#:new-marker new-marker any/c '#:new])
         sexp?]{
 Produces a tree that corresponds to the common structure of @racket[e1] and
 @racket[e2], with @racket[e1]-specific parts tagged with @racket[old-marker]
 and  @racket[e2]-specific parts tagged with @racket[new-marker].
            
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
          (sexp-diff
           #:old-marker '#:expected #:new-marker '#:actual
           '(1 2 3 4)
           '(1 2 2 4))
	   ]
}
