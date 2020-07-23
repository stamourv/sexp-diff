#lang racket/base

;;; sexp-diff.rkt -- diffs s-expressions based on Levenshtein-like edit distance.
;;; Ported more or less directly from Michael Weber's Common Lisp implementation.

;; This code is in the Public Domain.

;;; Description:

;; sexp-diff computes a diff between two s-expressions which minimizes
;; the number of atoms in the result tree, also counting edit
;; conditionals #:new, #:old.

;;; Examples:

(module+ test
  (require rackunit)

  (check-equal?
   (sexp-diff
    '(define (f x) (+ (* x 2) 1))
    '(define (f x) (- (* x 2) 3 1)))
   '((define (f x) (#:new - #:old + (* x 2) #:new 3 1))))

  (check-equal?
   (sexp-diff
    '(define (f x) (+ (* x 2) 4 1))
    '(define (f x) (- (* x 2) 5 3 1)))
   '((define (f x) (#:new - #:old + (* x 2) #:new 5 #:new 3 #:old 4 1))))

  (check-equal?
   (sexp-diff
    '(define (f x) (+ (* x 2) 4 4 1))
    '(define (f x) (- (* x 2) 5 5 3 1)))
   '((define (f x)
       (#:new - #:old + (* x 2) #:new 5 #:new 5 #:new 3 #:old 4 #:old 4 1))))

  (check-equal?
   (sexp-diff
    #:old-marker '#:expected #:new-marker '#:actual
    '(1 2 3 4)
    '(1 2 2 4))
   '((1 #:actual 2 2 #:expected 3 4)))

  (check-equal?
   (sexp-diff
    '(define (f x) (+ (* x 2) 1))
    '(define (f x) (- (* x 2) 3 1))
    #:old-marker (lambda (x)
                   `((highlight:old ,x)))
    #:new-marker (lambda (x)
                   `((highlight:new ,x))))
   '((define (f x) ((highlight:new -) (highlight:old +) (* x 2) (highlight:new 3) 1))))
  )

;;; Todo:

;; * Support for moved subtrees
;; * The algorithm treats vectors, arrays, etc. as opaque objects
;; * This article might describe a better method (unchecked):
;;   Hélène Touzet: "A linear tree edit distance algorithm for similar ordered trees"
;;   LIFL - UMR CNRS 8022 - Université Lille 1
;;   59 655 Villeneuve d'Ascq cedex, France
;;   Helene.Touzet@lifl.fr
;; * Format for reporting differences in improper lists is clunky


;;; Code:

(require
 racket/list
 "utils.rkt")

(provide sexp-diff)

(define size (tree-size pair? map))

(define make-deletion-record
  (make-deletion-record-constructor size))

(define make-insertion-record
  (make-insertion-record-constructor size))

(define make-update-record
  (make-update-record-constructor size))

(define make-unchanged-record
  (make-unchanged-record-constructor size))

(define-values (make-compound-record
                make-empty-compound-record
                make-extend-compound-record)
  (make-compound-record-constructors map))

(define initial-distance
  (make-initial-distance make-empty-compound-record
                         make-extend-compound-record))

(define (render-difference record old-marker new-marker)
  (cond [(insertion-record? record)
         (new-marker (insertion-record-change record))]
        [(deletion-record? record)
         (old-marker (deletion-record-change record))]
        [(update-record? record)
         `(,@(old-marker (update-record-old record))
           ,@(new-marker (update-record-new record)))]
        [(unchanged-record? record)
         (list (unchanged-record-change record))]
        [(compound-record? record)
         (list (for/fold ((res '()))
                   ((r (reverse (compound-record-changes record))))
                 (append res (render-difference r old-marker new-marker))))]))

;; Calculates the minimal edits needed to transform OLD-TREE into NEW-TREE.
;; It minimizes the number of atoms in the result tree, also counting
;; edit conditionals.
(define (levenshtein-tree-edit old-tree new-tree)
  (cond
    ((equal? old-tree new-tree)
     (make-unchanged-record old-tree))
    ((not (and (pair? old-tree) (pair? new-tree)))
     (make-update-record old-tree new-tree))
    (else
     (min/edit
      (make-update-record old-tree new-tree)
      (let* ((best-edit #f)
             (row (initial-distance make-deletion-record old-tree))
             (col (initial-distance make-insertion-record new-tree)))
        (for ((new-part (in-list new-tree))
              (current (in-list (drop (vector->list col) 1))))
          (for ((old-part (in-list old-tree))
                (row-idx  (in-naturals)))
            (set! best-edit (min/edit (make-extend-compound-record (vector-ref row (add1 row-idx))
                                                                   (make-insertion-record new-part))
                                      (make-extend-compound-record current
                                                                   (make-deletion-record old-part))
                                      (make-extend-compound-record (vector-ref row row-idx)
                                                                   (levenshtein-tree-edit old-part new-part))))
            (vector-set! row row-idx current)
            (set! current best-edit))
          (vector-set! row (sub1 (vector-length row)) best-edit))
        best-edit)))))

;; Computes a diff between OLD-TREE and NEW-TREE which minimizes the
;; number of atoms in the result tree, also counting inserted edit conditionals
;; #:new, #:old.
(define (sexp-diff old-tree new-tree
                   #:old-marker [old-marker '#:old]
                   #:new-marker [new-marker '#:new])
  (let ([old-marker-proc
         (if (procedure? old-marker)
             old-marker
             (lambda (x)
               `(,old-marker ,x)))]
        [new-marker-proc
         (if (procedure? new-marker)
             new-marker
             (lambda (x)
               `(,new-marker ,x)))])
    (render-difference (levenshtein-tree-edit old-tree new-tree)
                       old-marker-proc new-marker-proc)))
