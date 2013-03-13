#lang scheme

(define atom?
  (lambda (x)
  (and (not (pair? x)) (not (null? x)))))

;; NUMBERS
;; Page 59

(define add1
  (lambda (n)
    (+ n 1)))

(add1 67)

(define sub1
  (lambda (n)
    (- n 1)))

(sub1 5)
(sub1 0)

(zero? 0)
(zero? 1492)


(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
        ;;(print n)
        (add1 (o+ n (sub1 m)))))))

(o+ 4 4)

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(o- 17 7)

;; Tuples

;;(tup? (1 32 312))


;; page 64

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(addtup '(12 12 12))


(define o*
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else
        (o+ n (o* n (sub1 m)))))))

(o* 5 5)

;; (x 5 5)  = 5 + (x 5 4)
;;      = 5 + 5 + (x 5 3)
;;      = 5 + 5 + 5 + (x 5 2)
;;      = 5 + 5 + 5 + 5 + (x 5 1)
;;      = 5 + 5 + 5 + 5 + 5 + (x 5 0)
;;      = 5 + 5 + 5 + 5 + 5 + 0

;; page 69

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
        (cons (o+ (car tup1) (car tup2))
          (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(2 2) '(4 4 4))

;; page 71

(> 12 133)

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
        (o> (sub1 n) (sub1 m))))))

(o> 134 133)

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
        (o< (sub1 n) (sub1 m))))))

(o< 6 5)


;; page 74

(define o=?
  (lambda (n m)
    (cond
      ((zero? m) (zero? n))
      ((zero? n) #f)
      (else (o=? (sub1 n) (sub1 m))))))

(o=? 4 4)

(define o=
  (lambda (n m)
    (cond
      ((o> n m) #f)
      ((o< n m) #f)
      (else #t))))

(o= 4 4)

(define o**
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (o* n (o** n (sub1 m)))))))

(o** 5 3)


(define o/
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (o/ (o- n m) m))))))

;; (/ 15 4) = 1 + (/ 11 4)
;;      = 1 + 1 + (/ 7 4)
;;      = 1 + 1 + 1 (/ 3 4)
;;      = 1 + 1 + 1 + 0

(o/ 15 4)

;; page 76

(define length
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (add1 (length (cdr tup)))))))

(length '('ome 'twp 'tr 'four 23))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(pick 1 '(0 1 2 3 4 5 6))

;; page 77

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 1 '(0 1 2 3))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cond
          ((number? (car lat)) (no-nums (cdr lat)))
          (else (cons (car lat) (no-nums (cdr lat)))))))))

(no-nums '('atom 1 'thing 2 4 5 'best 'of))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cond
          ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
          (else (all-nums (cdr lat))))))))

(all-nums '('atom 1 'thing 2 4 5 'best 'of))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
        (o= a1 a2))
      ((or (number? a1) (number? a2)))
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
        ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
        (else (occur a (cdr lat))))))))

(occur 2 '(1 2 3 4 5 2 2))

(define one?
  (lambda (n)
    (o= n 1)))

(one? 1)

(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick2 (sub1 n) (cdr lat)))))))

(rempick2 2 '('one 'two 'three 'four))


;; 5. Oh my gawd it's full of stars*
;; S-expressions deep traversal?
;; page 81

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? (car l) a) (rember* a (cdr l)))
          (else
            (cons (car l) (rember* a (cdr l))))))
      (else
        (cons (rember* a (car l)) (rember* a (cdr l)))))))

(rember* 1 '(1 2 3 4 5))

;; page 82

(define insertR*
  (lambda (new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
      (cond
        ((eq? (car l) old)
         (cons old
           (cons new (insertR* new old (cdr l)))))
        (else
         (cons (car l) (insertR* new old (cdr l))))))
    (else (cons (insertR* new old (car l) (insertR* new old (cdr l))))))))

(insertR* 'hehe 'huehuehue '(12 21 test hehe huehuehue))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
        (cond
          ((eq? (car l) a)
            (add1 (occur* a (cdr l))))
          (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(occur* 'banana '(banana banana test (test (banana))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? (car l) old)
            (cons new (cdr l)))
          (else
            (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(subst* 'banana 'chowder '(what a delicious chowder))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? (car l) old)
            (cons new (cons old (insertL* new old (cdr l)))))
          (else
            (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l) (insertL* new old (cdr l))))))))

(insertL* 'banana 'chowder '(what a delicious chowder))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
        (or
          (eq? (car l) a)
          (member* a (cdr l))))
      (else
        (or
          (member* a (car l))
          (member* a (cdr l)))))))

(member* 'chowder '(what a delicious chowder))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(leftmost '((leftiest) (yes (test test))))

; or   -   asks questions one at a time until it finds one that is true
;          returns bool

; page 91

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((and (null? l1) (atom? (car l2))) #f)
      ((null? l1) #f)
      ((and (atom? (car l1)) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      ((atom? (car l1)) #f)
      ((null? l2) #f)
      ((atom? (car l2)) #f)
      (else
        (and (eqlist? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? '() '())
(eqlist? '() '(abc))
(eqlist? '(abc) '(abc))
(eqlist? '() '(()))
(eqlist? '(()) '())
