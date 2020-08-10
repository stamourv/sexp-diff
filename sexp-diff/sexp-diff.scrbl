#lang scribble/doc

@(require
  scribble/eval
  scribble/manual
  (for-label
   racket/base
   syntax/parse
   racket/contract))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require sexp-diff syntax/parse))
    the-eval))

@begin[(require (for-label sexp-diff))]
@(declare-exporting sexp-diff)

@title[#:tag "top"]{sexp-diff}
@author[
@author+email["Vincent St-Amour" "stamourv@racket-lang.org"]
@author+email["William J. Bowman" "wjb@williamjbowman.com"]
]

This package provides an S-expression-aware diffing tool based on
Levenshtein-like tree edit distance.

@defproc[(sexp-diff [e1 sexp?] [e2 sexp?]
                    [#:old-marker old-marker (or/c any/c (-> any/c list?)) '#:old]
                    [#:new-marker new-marker (or/c any/c (-> any/c list?)) '#:new])
         sexp?]{
 Produces a tree that corresponds to the common structure of @racket[e1] and
 @racket[e2], with @racket[e1]-specific parts tagged with @racket[old-marker]
 and @racket[e2]-specific parts tagged with @racket[new-marker].

 If either @racket[old-marker] or @racket[new-marker] is a @racket[procedure?],
 then it will be called on the node instead of inserted as a literal in the
 tree.
 This can be used to replace the node with a new s-expression whose head is the
 marker, rather than inserting the marker as a sibling of the node, enabling the
 marker to be interpreted as a function in some DSL.
 It may make more sense to use this feature in @racket[stx-diff].

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
          (sexp-diff
           #:old-marker (lambda (x) `((highlight:old ,x)))
           #:new-marker (lambda (x) `((highlight:new ,x)))
           '(1 2 3 4)
           '(1 2 2 4))
           (sexp-diff
            '((1) 2 3 4)
            '([1] 2 2 4))
	   ]

}

@defproc[(stx-diff [e1 syntax?] [e2 syntax?]
                   [#:old-marker old-marker (or/c any/c (-> any/c syntax?)) '#:old]
                   [#:new-marker new-marker (or/c any/c (-> any/c syntax?)) '#:new])
         syntax?]{
 Produces a syntax object that corresponds to the common structure of @racket[e1] and
 @racket[e2], with @racket[e1]-specific parts tagged with @racket[old-marker]
 and @racket[e2]-specific parts tagged with @racket[new-marker].

 The algorithm ignores syntax properties and source location when determining
 equality, instead comparing up to @racket[free-identifier=?], but attempts to
 reconstruct source locations and syntax properties in the generated syntax object.

 If either @racket[old-marker] or @racket[new-marker] is a @racket[procedure?],
 then it is in essence a macro that will transformer node instead of inserting a
 a literal as a sibling in the syntax object.
 The marker procedure must return a syntax object that represents a list.
}
