#lang racket
;; Lambdas

(define my-or
  (lambda (a b)
    (or a b)
  )
)

(define (my-or-2 a b)
  (or a b)
)

;;; (equal? (my-or #t #f) #t)
;;; (my-or-2 #t (+ 5 ""))

;;; (car '(1 . 2))
;;; (cdr '(1 . (1, 2, 3)))
;;; (cons 1 (cons 2 3))
;;; (cons 1 '(1, 2, 3, 4, 5))
;;; (cons '((1 . 2), (3 . 4)) '())


;;; (if #t 1 2)
;;; (cond 
;;;   [#f 2]
;;;   [#t 1]
;;; )

;;; ((lambda (x y) (+ x y)) 1 2)


(define (my-map op lst)
  (cond
    [(empty? lst) '()]
    [else (cons (op (car lst)) (my-map op (cdr lst)))]
  )
)

(define (my-fold op id lst)
  (cond 
    [(empty? lst) id]
    [else (op (car lst) (my-fold op id (cdr lst)))]
  )
)

;;; (define (apply op lst)
;;;   (cond
;;;     [(empty? lst)]
;;;     [else ()]
;;;   )
;;; )


;;; (my-fold + 0 '(1 2 3 4))
;;; (my-map abs '(-1 1 -3 2))
;;; (apply empty? '((1 2 3)))



; Define append using foldr : (append '(1 2 3) '(4 5 6)) -> '(1 2 3 4 5 6)
(define (append lst1 lst2)
  (foldr cons lst2 lst1)
)

;;; (append '(1 2 3) '(4 5 6))

;;; (define (mapfold op lst)
;;;   (foldr op '() lst)
;;; )

;;; (mapfold abs '(1 -1 -3 2))


(define local
  (let ([x 6])
    (+ x 7))
)

;;; (+ local 0)

(define x 10)
(set! x 7)
;;; (+ x 0)


(define (any? p xs)
  (cond
    [(empty? xs) #f]
    [(equal? #t (p (first xs))) #t]
    [else (any? p (rest xs))]
  )
)


(define (any?-fold p xs)
  (foldr (lambda (x y) (or (p x) y)) #f xs)
)

;;; (any? even? '())
;;; (any?-fold even? '(1 4 5))


;; Factorial

;; Regular
(define (fact n)
  (cond
    [(equal? n 1) 1]
    [else (* n (fact (- n 1)))]
  )
)

(fact 3)

;; Tail Recursive
;;; (define fact-tail
;;;   (lambda (n)
;;;     (local [(define ft
;;;           (lambda (n a) 
;;;             (cond 
;;;               [(equal? n 1) a]
;;;               [else (ft (- n 1) (* n a))]
;;;             )
;;;           )      
;;;       )
;;;     ]
;;;     (ft n 1) 
;;;     )
;;;   )
;;; )

;;; (define fact-tail
;;;   (lambda (n)
;;;     (local [(define ft
;;;       (lambda (n a) 
;;;         (if (= n 0)
;;;           a
;;;           (ft (- n 1) (* n a)))))]
;;;     (ft n 1))))



;;; (fact-tail 3)


;;; (define adder 
;;;   (lambda (n)
;;;     (local [
;;;       (define add-tail
;;;         (lambda (n a)
;;;           (cond 
;;;             [(equal? n 1) 1]
;;;             [else (add-tail (- n 1) (+ n a))]
;;;           )
;;;         )
;;;       )
;;;     ]
;;;     (add-tail n 0))
;;;   )
;;; )

(define fact-cps
  (lambda (n)
    (local [
      (define fcps
        (lambda (n k)
          (cond
            [(equal? n 0) (k 1)]
            [else (fcps (- n 1) (lambda (v) (k (* n v))))]
          )
        )
      )
    ]
    (fcps n 1))
  )
)


(fact-cps 10)