#lang racket/base

;;; stx-diff.rkt -- diffs syntax objects based on Levenshtein-like edit distance.
;;; Ported more or less directly from Michael Weber's Common Lisp implementation.

;; This code is in the Public Domain.

;;; Description:

;; stx-diff computes a diff between two syntax objects which minimizes
;; the number of atoms in the result tree, also counting edit
;; conditionals #:new, #:old, and attempts to preserve syntax properties and
;; maintain sensible source location information.

;;; Examples:

(module+ test
  (require rackunit)

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
  )

;;; Code:

(require
 syntax/stx
 racket/list
 syntax/srcloc
 "utils.rkt")

(provide stx-diff)

(define stx-size (tree-size stx-pair? stx-map))

(define make-deletion-record
  (make-deletion-record-constructor stx-size))

(define make-insertion-record
  (make-insertion-record-constructor stx-size))

(define make-update-record
  (make-update-record-constructor stx-size))

(define make-unchanged-record
  (make-unchanged-record-constructor stx-size))

(define-values (make-compound-record
                make-empty-compound-record
                make-extend-compound-record)
  (make-compound-record-constructors stx-map))

(struct compound-stx-record compound-record (props srcloc))

(define (make-inherit-compound-stx-record props srcloc super)
  (compound-stx-record
   (edit-record-edit-distance super)
   (compound-record-changes super)
   props
   srcloc))

(define (make-compound-stx-record props srcloc changes)
  (make-inherit-compound-stx-record
   props srcloc
   (make-compound-record changes)))

(define (make-empty-compound-stx-record props srcloc)
  (make-inherit-compound-stx-record
   props srcloc
   (make-empty-compound-record)))

(define (make-extend-compound-stx-record props srcloc r0 record)
  (make-inherit-compound-stx-record
   props srcloc
   (make-extend-compound-record r0 record)))

(require racket/function)
(define initial-distance
  (make-initial-distance (lambda ()
                           (make-empty-compound-stx-record '()
                                (build-source-location-syntax #f)))
                         (curry make-extend-compound-stx-record '()
                                (build-source-location-syntax #f))))

#;(define (initial-distance function stx)
  (let ([lst (syntax->list stx)])
    (let ((seq (make-vector (add1 (length lst)) (make-empty-compound-stx-record '() #f))))
      (for ((i   (in-naturals))
            (elt (in-list lst)))
        (vector-set! seq (add1 i)
                     (make-extend-compound-stx-record
                      (syntax-property )
                      (vector-ref seq i)
                      (function elt))))
      seq)))

(require racket/dict)
(define (extract-props stx)
  (for/fold ([d '()])
            ([key (syntax-property-symbol-keys stx)])
    (dict-set d key (syntax-property stx key))))

(define (assign-props props stx)
  (for/fold ([stx stx])
            ([(key value) (in-dict props)])
    (syntax-property stx key value)))

(define (render-difference record old-marker new-marker)
  (cond [(insertion-record? record)
         (quasisyntax/loc (insertion-record-change record)
           (#,@(new-marker (insertion-record-change record))))]
        [(deletion-record? record)
         (quasisyntax/loc (build-source-location-syntax #f)
           (#,@(old-marker (deletion-record-change record))))]
        [(update-record? record)
         (quasisyntax/loc (update-record-new record)
           (#,@(old-marker (update-record-old record))
            #,@(new-marker (update-record-new record))))]
        [(unchanged-record? record)
         (quasisyntax/loc (unchanged-record-change record)
           (#,(unchanged-record-change record)))]
        [(compound-stx-record? record)
         (quasisyntax/loc (compound-stx-record-srcloc record)
           (#,(for/fold ((res '()))
                        ((r (reverse (compound-record-changes record))))
                (let ([c (render-difference r old-marker new-marker)])
                  (assign-props
                   (compound-stx-record-props record)
                   (quasisyntax/loc (compound-stx-record-srcloc record)
                     (#,@res #,@c)))))))]))

;; Calculates the minimal edits needed to transform OLD-TREE into NEW-TREE.
;; It minimizes the number of atoms in the result tree, also counting
;; edit conditionals.
(define (maybe/free-identifier=? id1 id2)
  (and (identifier? id1) (identifier? id2) (free-identifier=? id1 id2)))

(define (levenshtein-stx-edit old-stx new-stx)
  (cond
    ((maybe/free-identifier=? old-stx new-stx)
     (make-unchanged-record new-stx))
    ((and
      (or (not (identifier? old-stx))
          (not (identifier? new-stx)))
      (equal? (syntax->datum old-stx)
              (syntax->datum new-stx)))
     (make-unchanged-record new-stx))
    ((not (and (stx-pair? old-stx) (stx-pair? new-stx)))
     (make-update-record old-stx new-stx))
    (else
     (min/edit
      (make-update-record old-stx new-stx)
      (let* ((best-edit #f)
             (row (initial-distance make-deletion-record (syntax->list old-stx)))
             (col (initial-distance make-insertion-record (syntax->list new-stx))))
        (for ((new-part (in-list (syntax->list new-stx)))
              (current (in-list (drop (vector->list col) 1))))
          (for ((old-part (in-list (syntax->list old-stx)))
                (row-idx  (in-naturals)))
            (set! best-edit (min/edit (make-extend-compound-stx-record
                                       (extract-props new-stx)
                                       new-stx
                                       (vector-ref row (add1 row-idx))
                                       (make-insertion-record new-part))
                                      (make-extend-compound-stx-record
                                       (extract-props old-stx)
                                       new-stx
                                       current
                                       (make-deletion-record old-part))
                                      (make-extend-compound-stx-record
                                       (extract-props new-stx)
                                       new-stx
                                       (vector-ref row row-idx)
                                       (levenshtein-stx-edit old-part new-part))))
            (vector-set! row row-idx current)
            (set! current best-edit))
          (vector-set! row (sub1 (vector-length row)) best-edit))
        best-edit)))))

;; Computes a diff between OLD-STX and NEW-STX which minimizes the
;; number of atoms in the result tree, also counting inserted edit conditionals
;; #:new, #:old.
(define (stx-diff old-stx new-stx
                  #:old-marker [old-marker '#:old]
                  #:new-marker [new-marker '#:new])

  (let ([old-marker-proc
         (if (procedure? old-marker)
             old-marker
             (lambda (x)
               (quasisyntax/loc x
                 (#,old-marker #,x))))]
        [new-marker-proc
         (if (procedure? new-marker)
             new-marker
             (lambda (x)
               (quasisyntax/loc x
                 (#,new-marker #,x))))])
    (render-difference (levenshtein-stx-edit old-stx new-stx)
                     old-marker-proc new-marker-proc)))
