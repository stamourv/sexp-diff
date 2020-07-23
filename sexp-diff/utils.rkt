#lang racket/base

(provide (all-defined-out))

(struct edit-record (edit-distance))

(struct unchanged-record edit-record (change))

(struct deletion-record edit-record (change))

(define ((make-deletion-record-constructor tree-size) change)
  (deletion-record (add1 (tree-size change)) change))

(struct insertion-record edit-record (change))

(define ((make-insertion-record-constructor tree-size)
         change)
  (insertion-record (add1 (tree-size change)) change))

(struct update-record edit-record (old new))

(define ((make-update-record-constructor
          tree-size) old new)
  (update-record (+ 1 (tree-size old)
                    1 (tree-size new))
                 old new))

(define ((make-unchanged-record-constructor
          tree-size) change)
  (unchanged-record (tree-size change) change))

(struct compound-record edit-record (changes))

(define (make-compound-record-constructors
          tree-map)
  (let ([make-compound-record
         (lambda (changes)
           (compound-record (apply + (tree-map edit-record-edit-distance changes)) changes))])
    (values
     make-compound-record
     (lambda () (make-compound-record '()))
     (lambda (r0 record)
       (make-compound-record (cons record (get-change r0)))))))

(define (get-change record)
  (cond [(unchanged-record? record) (unchanged-record-change record)]
        [(deletion-record?  record) (deletion-record-change  record)]
        [(insertion-record? record) (insertion-record-change record)]
        [(compound-record?  record) (compound-record-changes record)]))

;; Computes the number of atoms contained in TREE.
(define ((tree-size [tree? pair?] [tree-map map])
         tree)
  (let loop ([tree tree])
    (if (tree? tree)
        (apply + 1 (tree-map loop tree))
        1)))

;; Returns record with minimum edit distance.
(define (min/edit record . records)
  (foldr (lambda (a b) (if (<= (edit-record-edit-distance a)
                               (edit-record-edit-distance b))
                           a b))
         record records))

;; Prepares initial data vectors for Levenshtein algorithm from LST.
(define ((make-initial-distance make-empty-compound-record make-extend-compound-record)
         function lst)
  (let ((seq (make-vector (add1 (length lst)) (make-empty-compound-record))))
    (for ((i   (in-naturals))
          (elt (in-list lst)))
      (vector-set! seq (add1 i)
                   (make-extend-compound-record (vector-ref seq i)
                                                (function elt))))
    seq))
