(define (not x) (if x #f #t))
(define (null? x) (eqv? x '()))

(define (list . xs) xs)

(define (id x) x)

(define (flip f) (lambda (x y) (f y x)))

(define (curry f x) (lambda (y) (apply f (cons x (list y)))))

(define (compose f g) (lambda (x) (f (apply g x))))

(define zero?       (curry = 0))
(define positive?   (curry < 0))
(define negative?   (curry > 0))
(define (odd? num)  (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))

(define (foldr f z xs)
  (if (null? xs)
      z
      (f (car xs) (foldr f z (cdr xs)))))

(define (foldl f accum xs)
  (if (null? xs)
      accum
      (foldl f (f accum (car xs)) (cdr xs))))

(define fold foldl)
(define reduce foldl)

(define (unfold f init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold f (f init) pred))))

(define (sum . xs)     (fold + 0 xs))
(define (product . xs) (fold * 1 xs))

(define (and . xs)     (fold && #t xs))
(define (or . xs)      (fold || #f xs))

(define (max x . xs) (fold (lambda (old new) (if (> old new) old new)) x xs))
(define (min x . xs) (fold (lambda (old new) (if (< old new) old new)) x xs))

(define (length lst) (fold (lambda (x y) (+ x 1)) 0 lst))

(define (reverse lst)(fold (flip cons) '() lst))

(define (mem-helper pred op)
  (lambda (acc next)
    (if (and (not acc) (pred (op next)))
        next
        acc)))
(define (memq obj lst)    (fold (mem-helper (curry eq? obj)    id)  #f lst))
(define (memv obj lst)    (fold (mem-helper (curry eqv? obj)   id)  #f lst))
(define (member obj lst)  (fold (mem-helper (curry equal? obj) id)  #f lst))
(define (assq obj alist)  (fold (mem-helper (curry eq? obj)    car) #f alist))
(define (assv obj alist)  (fold (mem-helper (curry eqv? obj)   car) #f alist))
(define (assoc obj alist) (fold (mem-helper (curry equal? obj) car) #f alist))

(define (map f xs) (foldr (lambda (x y) (cons (f x) y)) '() xs))

(define (filter p xs) ((foldr (lambda (x y) (if (p x) (cons x y) y)) '() xs))

