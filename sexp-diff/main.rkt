#lang racket/base

;;; sexp-diff.rkt -- diffs s-expressions based on Levenshtein-like edit distance.
;;; Ported more or less directly from Michael Weber's Common Lisp implementation.

;; This code is in the Public Domain.

;;; Description:

;; sexp-diff computes a diff between two s-expressions which minimizes
;; the number of atoms in the result tree, also counting edit
;; conditionals #:new, #:old.

;;; Examples:

;; > (sexp-diff
;;    '(DEFUN F (X) (+ (* X 2) 1)) 
;;    '(DEFUN F (X) (- (* X 2) 3 1)))
;; ((DEFUN F (X) (#:new - #:old + (* X 2) #:new 3 1)))
;; > (sexp-diff
;;    '(DEFUN F (X) (+ (* X 2) 4 1))
;;    '(DEFUN F (X) (- (* X 2) 5 3 1)))
;; ((DEFUN F (X) (#:new - #:old + (* X 2) #:new 5 #:new 3 #:old 4 1)))
;; > (sexp-diff
;;    '(DEFUN F (X) (+ (* X 2) 4 4 1))
;;    '(DEFUN F (X) (- (* X 2) 5 5 3 1)))
;; ((DEFUN F (X) #:new (- (* X 2) 5 5 3 1) #:old (+ (* X 2) 4 4 1)))

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

(require racket/list)

(provide sexp-diff)

;; Computes the number of atoms contained in TREE.
(define (tree-size tree)
  (if (pair? tree) 
      (apply + 1 (map tree-size tree))
      1))


(struct edit-record (edit-distance))

(struct unchanged-record edit-record (change))
(define (make-unchanged-record change)
  (unchanged-record (tree-size change) change))

(struct deletion-record edit-record (change))
(define (make-deletion-record change)
  (deletion-record (add1 (tree-size change)) change))

(struct insertion-record edit-record (change))
(define (make-insertion-record change)
  (insertion-record (add1 (tree-size change)) change))

(struct update-record edit-record (old new))
(define (make-update-record old new)
  (update-record (+ 1 (tree-size old)
                    1 (tree-size new))
                 old new))

(struct compound-record edit-record (changes))
(define (make-compound-record changes)
  (compound-record (apply + (map edit-record-edit-distance changes)) changes))
(define (make-empty-compound-record)
  (make-compound-record '()))
(define (make-extend-compound-record r0 record)
  (make-compound-record (cons record (get-change r0))))


(define (get-change record)
  (cond [(unchanged-record? record) (unchanged-record-change record)]
        [(deletion-record?  record) (deletion-record-change  record)]
        [(insertion-record? record) (insertion-record-change record)]
        [(compound-record?  record) (compound-record-changes record)]))

(define (render-difference record old-marker new-marker)
  (cond [(insertion-record? record)
         (list new-marker (insertion-record-change record))]
        [(deletion-record? record)
         (list old-marker (deletion-record-change record))]
        [(update-record? record)
         (list old-marker (update-record-old record) 
               new-marker (update-record-new record))]
        [(unchanged-record? record)
         (list (unchanged-record-change record))]
        [(compound-record? record)
         (list (for/fold ((res '()))
                   ((r (reverse (compound-record-changes record))))
                 (append res (render-difference r old-marker new-marker))))]))

;; Returns record with minimum edit distance.
(define (min/edit record . records)
  (foldr (lambda (a b) (if (<= (edit-record-edit-distance a)
                               (edit-record-edit-distance b))
                           a b))
         record records))


;; Prepares initial data vectors for Levenshtein algorithm from LST.
(define (initial-distance function lst)
  (let ((seq (make-vector (add1 (length lst)) (make-empty-compound-record))))
    (for ((i   (in-naturals))
          (elt (in-list lst)))
      (vector-set! seq (add1 i)
                   (make-extend-compound-record (vector-ref seq i)
                                                (function elt))))
    seq))

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
  (render-difference (levenshtein-tree-edit old-tree new-tree)
                     old-marker new-marker))
