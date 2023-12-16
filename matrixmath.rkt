;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname matrixmath) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; data structures


; A Vector is one of:
;   - '()
;   - cons (Number Vector)
(define (vector? v)
  (or
   (empty? v)
   (and
    (not (number? v))
    (number? (first v))
    (vector? (rest v)))))
; checks
(check-expect (vector? '()) #t)
(check-expect (vector? 7) #f)
(check-expect (vector? (cons 7 (cons 5 '()))) #t)
(check-expect (vector? (cons 7 (cons "5" '()))) #f)
#;
(define (fn-on-vector v)
  (cond
    [(not (vector? v)) (error "not a vector")]
    [(empty? v) ...]
    [else ... (first v) ... (fn-on-vector (rest v))]))
          

; A Matrix is one of:
;   - '()
;   - cons (Vector Matrix)
;   every Vector must be the same length
(define (matrix? m)
  (or
   (empty? m)
   (and
    (vector? (first m))
    (or
     (empty? (rest m))
     (= (length (first m)) (length (first (rest m)))))
    (matrix? (rest m)))))
; checks
(check-expect (matrix? '()) #t)
(check-expect (matrix? (cons 7 (cons 5 '()))) #f)
(check-expect (matrix? (cons (cons 7 (cons 5 '())) '())) #t)
(check-expect (matrix? (cons (cons (cons 1 '()) (cons 3 '())) '())) #f)
(check-expect (matrix? (cons (cons 7 (cons 5 '()))
                             (cons (cons 9 (cons 0 '())) '()))) #t)
(check-expect (matrix? (cons (cons 7 (cons 5 '()))
                             (cons (cons 0 '()) '()))) #f)
#;
(define (fn-on-matrix m)
  (cond
    [(not (matrix? m)) (error "not a matrix")]
    [(empty? m) ...]
    [else ... (first m) ... (fn-on-matrix (rest m))]))



; functions


(define (*mat m1 m2)
  ; Matrix Matrix -> Matrix
  ; multiplies two matrices together
  (cond
    [(or (not (matrix? m1)) (not (matrix? m2))) (error "not matrices")]
    [(or (empty? m1) (empty? m2)) '()]
    [(not (= (length m1) (length (first m2))))
     (error "matrices must be congruent")]
    [else (cons (mat*vec (transpose m1) (first m2))
                (*mat m1 (rest m2)))]))
; checks
(check-expect (*mat '() '()) '())
(check-error (*mat (cons 7 '()) (cons 8 '())) "not matrices")
(check-error (*mat
              (cons (cons 1 (cons 3 '())) (cons (cons 2 (cons 4 '())) '()))
              (cons (cons 1 '()) (cons (cons 3 '()) '())))
             "matrices must be congruent")
(check-expect (*mat
               (cons (cons 1 '()) (cons (cons 3 '()) '()))
               (cons (cons 1 (cons 3 '())) (cons (cons 2 (cons 4 '())) '())))
              (cons (cons 10 '()) (cons (cons 14 '()) '())))
(check-expect (*mat
               (cons (cons 1 (cons 3 '())) (cons (cons 2 (cons 4 '())) '()))
               (cons (cons 1 (cons 3 '())) '()))
              (cons (cons 7 (cons 15 '())) '()))
(check-expect (*mat 
               (cons (cons 1 (cons 4 (cons 7 '())))
                     (cons (cons 2 (cons 5 (cons 8 '()))) '()))
               (cons (cons 1 (cons 4 '()))
                     (cons (cons 2 (cons 5 '()))
                           (cons (cons 3 (cons 6 '())) '()))))
              (cons (cons 9 (cons 24 (cons 39 '())))
                    (cons (cons 12 (cons 33 (cons 54 '())))
                          (cons (cons 15 (cons 42 (cons 69 '()))) '()))))


(define (transpose m)
  ; Matrix -> Matrix
  ; transpose a matrix
  (cond
    [(empty? (first m)) '()]
    [else (cons (first* m) (transpose (rest* m)))]))
; checks
(check-expect (transpose
               (cons (cons 1 (cons 4 (cons 7 '())))
                     (cons (cons 2 (cons 5 (cons 8 '())))
                           (cons (cons 3 (cons 6 (cons 9 '()))) '()))))
              (cons (cons 1 (cons 2 (cons 3 '())))
                    (cons (cons 4 (cons 5 (cons 6 '())))
                          (cons (cons 7 (cons 8 (cons 9 '()))) '()))))
(check-expect (transpose
               (cons (cons 1 (cons 4 '()))
                     (cons (cons 2 (cons 5 '()))
                           (cons (cons 3 (cons 6 '())) '()))))
              (cons (cons 1 (cons 2 (cons 3 '())))
                    (cons (cons 4 (cons 5 (cons 6 '()))) '())))


(define (first* m)
  ; Matrix -> Vector
  ; removes the first row vector from a matrix
  (cond
    [(empty? m) '()]
    [else (cons (first (first m)) (first* (rest m)))]))
; checks
(check-expect (first* (cons (cons 7 '()) '())) (cons 7 '()))
(check-expect (first* (cons (cons 4 '()) (cons (cons 7 '()) '())))
              (cons 4 (cons 7 '())))
(check-expect (first* (cons (cons 1 (cons 4 (cons 7 '())))
                            (cons (cons 2 (cons 5 (cons 8 '())))
                                  (cons (cons 3 (cons 6 (cons 9 '()))) '()))))
              (cons 1 (cons 2 (cons 3 '()))))


(define (rest* m)
  ; Matrix -> Vector
  ; discards the first row vector of a matrix, returning the rest
  (cond
    [(empty? m) '()]
    [else (cons (rest (first m)) (rest* (rest m)))]))
; checks
(check-expect (rest* '()) '())
(check-expect (rest* (cons (cons 7 '()) '())) (cons '() '()))
(check-expect (rest* (cons (cons 1 (cons 4 (cons 7 '())))
                           (cons (cons 2 (cons 5 (cons 8 '())))
                                 (cons (cons 3 (cons 6 (cons 9 '()))) '()))))
              (cons (cons 4 (cons 7 '()))
                    (cons (cons 5 (cons 8 '()))
                          (cons (cons 6 (cons 9 '())) '()))))
(check-expect (rest* (cons (cons 7 '())
                           (cons (cons 8 '())
                                 (cons (cons 9 '()) '()))))
              (cons '() (cons '() (cons '() '()))))


(define (mat*vec m v)
  ; Matrix Vector -> Vector
  ; multiplies a Vector by a Matrix
  (cond
    [(empty? m) '()]
    [else (cons (*inner (first m) v) (mat*vec (rest m) v))]))
; checks
(check-expect (mat*vec
               (cons (cons 1 (cons 3 '()))  (cons (cons 3 (cons 1 '())) '()))
               (cons 1 (cons 3 '())))
              (cons 10 (cons 6 '())))
(check-expect (mat*vec
               (cons (cons 1 (cons 3 '())) (cons (cons 2 (cons 4 '())) '()))
               (cons 1 (cons 3 '())))
              (cons 10 (cons 14 '())))


(define (*inner v1 v2)
  ; Vector Vector -> Number
  ; inner product of two vectors
  (cond
    [(empty? v2) 0]
    [else (+ (* (first v1) (first v2)) (*inner (rest v1) (rest v2)))]))
; checks
(check-expect (*inner (cons 1 (cons 2 (cons 3 '())))
                      (cons 4 (cons 5 (cons 6 '()))))
              (+ (* 1 4) (* 2 5) (* 3 6)))


; actions

(define twix (cons (cons 1 (cons 4 (cons 7 '())))
                   (cons (cons 2 (cons 5 (cons 8 '())))
                         (cons (cons 3 (cons 6 (cons 9 '()))) '()))))

(*mat (transpose twix) twix)
(*mat twix (transpose twix))