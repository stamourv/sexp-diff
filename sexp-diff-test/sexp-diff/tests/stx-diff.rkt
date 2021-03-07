#lang racket/base

(require
 sexp-diff
 rackunit)

(check-equal?
 (syntax->datum
  (stx-diff
   #'(define (f x) (+ (* x 2) 1))
   #'(define (f x) (- (* x 2) 3 1))))
 '((define (f x) (#:new - #:old + (* x 2) #:new 3 1))))

(check-equal?
 (syntax->datum
  (stx-diff
   #'(define (f x) (+ (* x 2) 4 1))
   #'(define (f x) (- (* x 2) 5 3 1))))
 '((define (f x) (#:new - #:old + (* x 2) #:new 5 #:new 3 #:old 4 1))))

(check-equal?
 (syntax->datum
  (stx-diff
   #'(define (f x) (+ (* x 2) 4 4 1))
   #'(define (f x) (- (* x 2) 5 5 3 1))))
 '((define (f x)
     (#:new - #:old + (* x 2) #:new 5 #:new 5 #:new 3 #:old 4 #:old 4 1))))

(check-equal?
 (syntax->datum
  (stx-diff
   #:old-marker '#:expected #:new-marker '#:actual
   #'(1 2 3 4)
   #'(1 2 2 4)))
 '((1 #:actual 2 2 #:expected 3 4)))

(check-equal?
 (syntax->datum
  (stx-diff
   #'(define (f x) (+ (* x 2) 1))
   #'(define (f x) (- (* x 2) 3 1))
   #:old-marker (lambda (x)
                  #`((highlight:old #,x)))
   #:new-marker (lambda (x)
                  #`((highlight:new #,x)))))
 '((define (f x) ((highlight:new -) (highlight:old +) (* x 2) (highlight:new 3) 1))))

(require syntax/parse)
(let ([diff (stx-diff
             #'((1) 2 3 4)
             #'([1] 2 2 4))])
  (check-equal?
   (syntax-parse diff
     [((head any ...))
      (syntax-property #'head 'paren-shape)])
   #\[)

  (check-false
   (syntax-property diff 'paren-shape)))

(let ([diff
       (stx-diff
        #'(e integer? (let ([x e]) e))
        #'(e (let ([x v]) v) (v integer?)))])

  (check-false
   (syntax-parse diff
     [((_ _ _ (let bs _ ...) . _))
      (syntax-property #'bs 'paren-shape)]))

  (check-equal?
   (syntax-parse diff
     [((_ _ _ (let (b) _ ...) . _))
      (syntax-property #'b 'paren-shape)])
   #\[))
