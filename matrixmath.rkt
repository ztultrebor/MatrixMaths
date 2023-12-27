;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname matrixmath) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; data structures


; A Vector is one of:
;   - '()
;   - cons (Number Vector)
(define (vector? v)
  (and
   (list? v)
   (or
    (empty? v)
    (and
     (number? (first v))
     (vector? (rest v))))))
; checks
(check-expect (vector? '()) #t)
(check-expect (vector? 7) #f)
(check-expect (vector? '(7 5)) #t)
(check-expect (vector? '(7 "5")) #f)
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
  (and
   (list? m)
   (or
    (empty? m)
    (and 
     (vector? (first m))
     (matrix? (rest m))
     (or
      (empty? (rest m))
      (= (length (first m)) (length (first (rest m)))))))))
; checks
(check-expect (matrix? '()) #t)
(check-expect (matrix? 7) #f)
(check-expect (matrix? '(7 5)) #f)
(check-expect (matrix? `(,'( 7 5))) #t)
(check-expect (matrix? `(,`(,'(1) ,'(3)))) #f)
(check-expect (matrix? `('(1) `(,'(2) `(,'(3 3))))) #f)
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
(check-error (*mat '(7) '(8)) "not matrices")
(check-error (*mat `(,'(1 3) ,'(2 4)) `(,'(1) ,'(3)))
             "matrices must be congruent")
(check-expect (*mat `(,'(1) ,'(3)) `(,'(1 3) ,'(2 4))) `(,'(10) ,'(14)))
(check-expect (*mat `(,'(1 3) ,'(2 4)) `(,'(1 3))) `(,'(7 15)))
(check-expect (*mat `(,'(1 4 7) ,'(2 5 8)) `(,'(1 4) ,'(2 5) ,'(3 6)))
              `(,'(9 24 39) ,'(12 33 54) ,'(15 42 69)))


(define (transpose m)
  ; Matrix -> Matrix
  ; transpose a matrix
  (cond
    [(empty? (first m)) '()]
    [else (cons (first* m) (transpose (rest* m)))]))
; checks
(check-expect (transpose `(,'(1 4 7) ,'(2 5 8) ,'(3 6 9)))
              `(,'(1 2 3) ,'(4 5 6) ,'(7 8 9)))
(check-expect (transpose `(,'(1 4) ,'(2 5) ,'(3 6))) `(,'(1 2 3) ,'(4 5 6)))


(define (identityM n)
  ; Natural -> Matrix
  ; create an nxn identity matrix
  (build-list n (lambda (i)
                  (build-list n (lambda (j) (if (= j i) 1 0))))))
; checks
(check-expect (identityM 3) `(,'(1 0 0) ,'(0 1 0) ,'(0 0 1)))


(define (first* m)
  ; Matrix -> Vector
  ; removes the first row vector from a matrix
  (cond
    [(empty? m) '()]
    [else (cons (first (first m)) (first* (rest m)))]))
; checks
(check-expect (first* `(,'(7))) '(7))
(check-expect (first* `(,'(4) ,'(7))) '(4 7))
(check-expect (first* `(,'(1 4 7) ,'(2 5 8) ,'(3 6 9))) '(1 2 3))


(define (rest* m)
  ; Matrix -> Matrix
  ; discards the first row vector of a matrix, returning the rest
  (cond
    [(empty? m) '()]
    [else (cons (rest (first m)) (rest* (rest m)))]))
; checks
(check-expect (rest* '()) '())
(check-expect (rest* `(,'(7))) `(,'()))
(check-expect (rest* `(,'(1 4 7) ,'(2 5 8) ,'(3 6 9)))
              `(,'(4 7) ,'(5 8) ,'(6 9)))
(check-expect (rest* `(,'(7) ,'(8) ,'(9))) `(,'() ,'() ,'()))


(define (mat*vec m v)
  ; Matrix Vector -> Vector
  ; multiplies a Vector by a Matrix
  (cond
    [(empty? m) '()]
    [else (cons (*inner (first m) v) (mat*vec (rest m) v))]))
; checks
(check-expect (mat*vec `(,'(1 3) ,'(3 1)) '(1 3)) '(10 6))
(check-expect (mat*vec `(,'(1 3) ,'(2 4)) '(1 3)) '(10 14))


(define (*inner v1 v2)
  ; Vector Vector -> Number
  ; inner product of two vectors
  (cond
    [(empty? v2) 0]
    [else (+ (* (first v1) (first v2)) (*inner (rest v1) (rest v2)))]))
; checks
(check-expect (*inner '(1 2 3) '(4 5 6))  (+ (* 1 4) (* 2 5) (* 3 6)))


; actions

(define twix `(,'(2 3 4 5 1)
               ,'(4 5 1 2 3)
               ,'(1 2 3 4 5)
               ,'(5 1 2 3 4)
               ,'(3 4 5 1 2)))

(*mat (transpose twix) twix)
(*mat twix (transpose twix))
(*mat twix twix)

(identityM 5)