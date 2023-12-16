;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname matrixmath) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; data structures


; A Vector is one of:
;   - '()
;   - cons (Number Vector)
(define (vector? v)
  (or (empty? v) (and (number? (first v)) (vector? (rest v)))))
; checks
(check-expect (vector? '()) #t)
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
    (or
     (empty? (rest m))
     (= (length (first m)) (length (first (rest m)))))
    (vector? (first m))
    (matrix? (rest m)))))
; checks
(check-expect (matrix? '()) #t)
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

(define (transpose-matrix m)
  ; Matrix -> Matrix
  ; transpose a matrix
  (cond
    [(not (matrix? m)) (error "not a matrix")]
    [(empty? m) '()]
    [(empty? (rest m)) (transpose-vector (first m))]
    [else (hcat (transpose-vector (first m))
                (transpose-matrix (rest m)))]))
; checks
(check-expect (transpose-matrix (cons (cons 7 (cons 5 '()))
                                      (cons (cons 9 (cons 0 '())) '())))
              (cons (cons 7 (cons 9 '()))
                    (cons (cons 5 (cons 0 '())) '())))
(check-expect (transpose-matrix (cons (cons 7 (cons 5 (cons 4 '())))
                                      (cons (cons 9 (cons 0 (cons 1 '()))) '())))
              (cons (cons 7 (cons 9 '()))
                    (cons (cons 5 (cons 0 '()))
                          (cons (cons 4 (cons 1 '())) '()))))


(define (transpose-vector v)
  ; Vector -> Vector
  ; transpose a matrix
  (cond
    [(not (vector? v)) (error "not a vector")]
    [(empty? v) '()]
    [else (cons (cons (first v) '()) (transpose-vector (rest v)))]))
; checks
(check-expect (transpose-vector (cons 1 (cons 2 (cons 3 '()))))
              (cons (cons 1 '()) (cons (cons 2 '()) (cons (cons 3 '()) '()))))


(define (hcat m1 m2)
  ; Matrix Matrix -> Matrix
  ; horizontal concatenation of a pxn matrix with a qxn matrix
  (cond
    [(empty? m1) '()]
    [else (cons (cons (first (first m1)) (first m2))
                (hcat (rest m1) (rest m2)))]))
; check
(check-expect (hcat (cons (cons 1 '()) (cons (cons 2 '()) (cons (cons 3 '()) '())))
                    (cons (cons 1 '()) (cons (cons 2 '()) (cons (cons 3 '()) '()))))
              (cons (cons 1 (cons 1 '()))
                    (cons (cons 2 (cons 2 '()))
                          (cons (cons 3 (cons 3 '())) '()))))


; actions

(define twix (cons (cons 1 (cons 2 (cons 3 '())))
              (cons (cons 4 (cons 5 (cons 6 '())))
              (cons (cons 7 (cons 8 (cons 9 '()))) '()))))

(transpose-matrix twix)