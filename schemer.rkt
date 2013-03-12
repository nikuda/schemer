#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? (quote ()))

(define l
  (list 'atomk 'one 'two 'three))

(car l)
(cdr l)
(car (cdr (cdr l)))

(cons 'first l)
(cons 'morefirst l)

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a 
              (cdr lat)))))))

(rember 'two l)

(define ll
  (list (list 'art 'bar) (list 'mar 'tar)))

; Page 45
(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l)) (firsts (cdr l)) )) )))

(display ll)
(firsts ll)

; Page 48
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond 
          ((eq? (car lat) old) (cons old (cons new (cdr lat))))
          (else (cons (car lat) (insertR new old (cdr lat)))))))))

(define lp
  (list 'are 'you 'a 'stink 'face))

(insertR 'fark 'stink lp)

; Page 51
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) old) (cons new (cons old (cdr lat))))
          (else (cons (car lat) (insertL new old (cdr lat)))))))))

(insertL 'test 'stink lp)

(define subst
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote ()))
      (else (cond
          ((eq? (car lat) old)
            (cons new (cdr lat)))
          (else (cons 
                  (car lat) 
                  (subst new old (cdr lat)))))))))

(subst 'maa 'stink lp)

; Page 52
(define subst2
  (lambda (new old1 old2 lat)
    (cond 
      ((null? lat) (quote ()))
      (else (cond
          ((eq? (car lat) old1)
            (cons new (cdr lat)))
          ((eq? (car lat) old2)
            (cons new (cdr lat)))
          (else (cons 
                  (car lat) 
                  (subst2 new old1 old2 (cdr lat)))))))))

(subst2 'maaaa 'face 'stink lp)

; not finished
(define multirember
  (lambda (a lat)
    (cond ((null? lat) (quote ()))
    (else (cond ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))))

; pass a list directly into function ??
(define multilist
  (list 'maa 'faa 'rra 'maa 'daa 'taa 'taa 'tre 'maa 'aaa))

(multirember 'taa multilist)


; page 56
(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
    (else (cond ((eq? (car lat) old)
                      (cons new (cons old (multiinsertR new old (cdr lat)))))
          (else (cons (car lat)
                      (multiinsertR new old (cdr lat)))))))))

(multiinsertR 'yes 'maa multilist)


(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
    (else (cond ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
          (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))

(define abcd
  (list 'chips 'and 'fish 'or 'fish 'and 'fried))

(multiinsertL 'fried 'fish abcd)

; page 57

(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
    (else (cond ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
          (else (cons (car lat) (multisubst new old (cdr lat)))))))))

(multisubst 'crabs 'fish abcd)


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

(rempick2 2 '(one two three four))


;; 5. Oh my gawd it's full of stars*
;; S-expressions deep traversal?
;; page 81

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? a (car l)) (rember* a (cdr l)))
          (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))


(rember* 2 '(2 3 (4 5) 6 8 9 (2 1 2 (2 3 3))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? old (car l))
            (cons old (cons new (insertR* new old (cdr l)))))
          (else
            (cons (car l) (insertR* new old (cdr l))))))
      (else
        (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(insertR* 'sir 'yes '(oh yes (oh yes (oh yes) oh yes)))


(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
        (cond
          ((eq? (car l) a)
            (add1 (occur* a (cdr l))))
          (else
            (occur* a (cdr l)))))
      (else
        (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(occur* 2 '(2 2 2 (2 3 4 (43 2))))


(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? (car l) old)
            (cons new (subst* new old (cdr l))))
          (else
            (cons (car l) (subst* new old (cdr l))))))
      (else
        (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(subst* 'yesterday 'today '(today was a nice day (yes today)))


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
      (else
        (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(insertL* 'before 'today '(today was a nice day (yes today)))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
        (or
          (eq? (car l) a)
          (member* a (cdr l))))
      (else
        (or (member* a (car l)) (member* a (cdr l)))))))

(member* 'yes '(today was a nice day (yes today)))


(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(leftmost '(((maybe) try this for a change (today) please)))


(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
        (and (eqan? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else
        (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? '(a b (aa)) '(a b (aa)))


(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
        (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(equal? '(a) '(ab))

; eqlist? rewrite using equal?
(define eqlist?r
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
        (and (equal? (car l1) (car l2))
          (eqlist?r (cdr l1) (cdr l2)))))))

(eqlist?r '(a b (aa)) '(a b (aa)))

; rember rewrite using equal?

(define remberr
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (remberr s (cdr l)))))))

(remberr 'test '(this is a big test))
