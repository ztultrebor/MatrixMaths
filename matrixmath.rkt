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


; actions

(define twix (cons (cons 1 (cons 4 '()))
                   (cons (cons 2 (cons 5 '()))
                         (cons (cons 3 (cons 6 '())) '()))))

(transpose twix)