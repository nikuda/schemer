#lang scheme

(provide (all-defined-out))

; atom? is not defined in most schemes because why?
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; is every sexp in list an atom?
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

; is atom in list?
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

; remove a list member
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a
              (cdr lat)))))))

; find first s-exp in each interal list
; Page 43
(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (cdr (car l)) (seconds (cdr l)))))))

; Page 48
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) old) (cons old (cons new (cdr lat))))
          (else (cons (car lat) (insertR new old (cdr lat)))))))))

; Page 51
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) old) (cons new (cons old (cdr lat))))
          (else (cons (car lat) (insertL new old (cdr lat)))))))))

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

; sub which comes first old1, or old2
; Page 52
(define subst2
  (lambda (new old1 old2 lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
          ; ((eq? (car lat) old1)
          ;   (cons new (cdr lat)))
          ; ((eq? (car lat) old2)
          ;   (cons new (cdr lat)))
          ((or (eq? (car lat) old1) (eq? (car lat) old2))
              (cons new (cdr lat)))
          (else (cons
                  (car lat)
                  (subst2 new old1 old2 (cdr lat)))))))))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) (quote ()))
    (else (cond ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))))

; page 56
(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
    (else (cond ((eq? (car lat) old)
                      (cons old (cons new (multiinsertR new old (cdr lat)))))
          (else (cons (car lat)
                      (multiinsertR new old (cdr lat)))))))))

(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
    (else (cond ((eq? (car lat) old)
                      (cons new (cons old (multiinsertL new old (cdr lat)))))
          (else (cons (car lat)
                      (multiinsertL new old (cdr lat)))))))))

; page 57
(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
    (else (cond ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
          (else (cons (car lat) (multisubst new old (cdr lat)))))))))


;; NUMBERS
;; Page 59

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
        ;;(print n)
        (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))


;; Tuples
;; page 64

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(define o*
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else
        (o+ n (o* n (sub1 m)))))))

;; page 69

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
        (cons (o+ (car tup1) (car tup2))
          (tup+ (cdr tup1) (cdr tup2)))))))


;; page 71

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
        (o> (sub1 n) (sub1 m))))))


(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
        (o< (sub1 n) (sub1 m))))))

;; page 74

(define o=?
  (lambda (n m)
    (cond
      ((zero? m) (zero? n))
      ((zero? n) #f)
      (else (o=? (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
      ((o> n m) #f)
      ((o< n m) #f)
      (else #t))))

(define o**
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (o* n (o** n (sub1 m)))))))

(define o/
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (o/ (o- n m) m))))))

;; page 76

(define length
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (add1 (length (cdr tup)))))))

; index
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

;; page 77

; remove by index
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

; remove numbers from list
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cond
          ((number? (car lat)) (no-nums (cdr lat)))
          (else (cons (car lat) (no-nums (cdr lat)))))))))

; remove non-numbers from list
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cond
          ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
          (else (all-nums (cdr lat))))))))

; is same atom?
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
        (o= a1 a2))
      ((or (number? a1) (number? a2))
        #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
        ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
        (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (o= n 1)))

; same as rempick but using one?
(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick2 (sub1 n) (cdr lat)))))))


;; 5. Oh my gawd it's full of stars*
;; S-expressions deep traversal?
;; page 81

; rember-star removes nested members
(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? a (car l)) (rember* a (cdr l)))
          (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

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

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

; or   -   asks questions one at a time until it finds one that is true
;          returns bool

; page 91

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

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
        (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

; eqlist? rewrite using equal?
(define eqlist?r
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
        (and (equal? (car l1) (car l2))
          (eqlist?r (cdr l1) (cdr l2)))))))

; rember rewrite using equal?
(define remberr
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (remberr s (cdr l)))))))

; page 99

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
        (and (numbered? (car aexp))
              (numbered? (car (cdr (cdr aexp)))))))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) 'o+)
        (o+ (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) 'o*)
        (o* (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp))))
      (else
        (o* (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp)))))))

; page 111

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset-old
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

; makeset using multirember
; preserves list order
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
        (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (subset set)
    (cond
      ((null? subset) #t)
      (else (and
        (member? (car subset) set)
        (subset? (cdr subset) set))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

; find any of atoms in set1 in set2
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(define intersect (lambda (set1 set2)
  (cond
    ((null? set1) '())
    ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
    (else (intersect (cdr set1) set2)))))

(define union (lambda (set1 set2)
  (cond
    ((null? set1) set2)
    ((member? (car set1) set2) (union (cdr set1) set2))
    (else (cons (car set1) (union (cdr set1) set2))))))

; set difference
; return all atoms from set1 that are not in set2
(define xxx (lambda (set1 set2)
  (cond
    ((null? set1) '())
    ((member? (car set1) set2) (xxx (cdr set1) set2))
    (else (cons (car set1) (xxx (cdr set1) set2))))))

(define intersect-all (lambda (l-set)
  (cond
    ((null? (cdr l-set)) (car l-set))
    (else (intersect (car l-set) (intersect-all (cdr l-set)))))))

(define a-pair? (lambda (x)
  (cond
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    ((null? (cdr (cdr x))) #t)
    (else #f))))

(define first (lambda (p) (car p)))
(define second (lambda (p) (car (cdr p))))
(define third (lambda (p) (car (cdr (cdr p)))))
(define build (lambda (s1 s2) (cons s1 (cons s2 (quote ())))))

(define fun? (lambda (rel) (set? (firsts rel))))

(define revpair (lambda (p)
  (build (second p) (first p))))

(define revrel (lambda (rel)
  (cond
    ((null? rel) (quote ()))
    (else (cons
      (revpair (car rel))
      (revrel (cdr rel)))))))

(define fullfun? (lambda (rel) (set? (seconds rel))))
(define one-to-one? (lambda (rel) (fun? (revrel rel))))

; Lambda the Ultimate

;(define rember-f (lambda (test? a l)
;  (cond
;    ((null? l) '())
;    ((test? (car l) a) (cdr l))
;    (else (cons (car l) (rember-f test? a (cdr l)))))))

(define rember-f (lambda (test?) (lambda (a l)
  (cond
    ((null? l) '())
    ((test? (car l) a) (cdr l))
    (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define insertL-f (lambda (test?) (lambda (new old l)
  (cond
    ((null? l) '())
    ((test? (car l) old) (cons new (cons old (cdr l))))
    (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertR-f (lambda (test?) (lambda (new old l)
  (cond
    ((null? l) '())
    ((test? (car l) old) (cons old (cons new (cdr l))))
    (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

(define seqL (lambda (new old l)
  (cons new (cons old l))))

(define seqR (lambda (new old l)
  (cons old (cons new l))))

(define insert-g (lambda (seq) (lambda (new old l)
  (cond
    ((null? l) '())
    ((eq? (car l) old) (seq new old (cdr l)))
    (else (cons (car l) ((insert-g seq) new old l)))))))

(define insertLr (insert-g seqL))
(define insertRr (insert-g seqR))

(define insertLrr (insert-g
  (lambda (new old l)
    (cons new (cons old l)))))

(define seqS (lambda (new old l) (cons new l)))

(define subst_r (insert-g seqS))

(define atom-to-function (lambda (x)
  (cond
    ((eq? x 'o+) o+)
    ((eq? x 'o*) o*)
    (else o**))))

(define value_r
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
        ((atom-to-function (operator nexp))
          (value_r (1st-sub-exp nexp))
          (value_r (2nd-sub-exp nexp)))))))

(define multirember_r (lambda (test?) (lambda (a lat)
  (cond
    ((null? lat) (quote ()))
    (else (cond
        ((test? (car lat) a) ((multirember_r test?) a (cdr lat)))
        (else (cons (car lat) ((multirember_r test?) a (cdr lat))))))))))

(define multirember-eq? (multirember_r eq?))

(define multiremberT (lambda (test? lat)
  (cond
    ((null? lat) (quote ()))
    (else (cond
        ((test? (car lat)) (multiremberT test? (cdr lat)))
        (else (cons (car lat) (multiremberT test? (cdr lat)))))))))

(define multirember&co (lambda (a lat col)
  (cond
    ((null? lat) (col (quote ()) (quote ())))
    ((eq? (car lat) a)
      (multirember&co a (cdr lat)
        (lambda (newlat seen)
          (col newlat (cons (car lat) seen)))))
    (else
      (multirember&co a (cdr lat)
        (lambda (newlat seen)
          (col (cons (car lat) newlat) seen)))))))

(define a-friend (lambda (x y) (null? y)))
(define last-friend (lambda (x y) (length x)))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) oldL)
        (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
        (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat)
        (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col (quote ()) 0 0))
      ((eq? (car lat) oldL)
        (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat l r)
          (col (cons new (cons oldL newlat)) (add1 l) r))))
      ((eq? (car lat) oldR)
        (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat l r)
          (col (cons oldR (cons new newlat)) l (add1 r)))))
      (else
        (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat l r)
          (col (cons (car lat) newlat) l r)))))))

(define even? (lambda (n)
  (o= (o* (o/ n 2) 2) n)))

(define evens-only* (lambda (l)
  (cond
    ((null? l) '())
    ((atom? (car l))
      (cond
        ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
        (else (evens-only* (cdr l)))))
    (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(define evens-only*&co (lambda (l col)
  (cond
    ((null? l) (col '() 1 0))
    ((atom? (car l))
      (cond
        ((even? (car l)) (evens-only*&co (cdr l) (lambda (newl prod sum)
          (col (cons (car l) newl) (o* (car l) prod) sum))))
        (else (evens-only*&co (cdr l) (lambda (newl prod sum)
          (col newl prod (o+ (car l) sum)))))))
    (else (evens-only*&co (car l) (lambda (newl prod sum)
      (evens-only*&co (cdr l) (lambda (newl2 prod2 sum2)
        (col (cons newl newl2) (o* prod prod2) (o+ sum sum2))))))))))

; not recurring on a part of lat
(define keep-looking (lambda (a sorn lat)
  (cond
    ((number? sorn) (keep-looking a (pick sorn lat) lat))
    (else (eq? sorn a)))))

(define looking (lambda (a lat)
  (keep-looking a (pick 1 lat) lat)))

(define shift (lambda (tup)
  (build (first (first tup)) (build (second (first tup)) (second tup)))))

(define align (lambda (tora)
  (cond
    ((atom? tora) tora)
    ((a-pair? (first tora)) (align (shift tora)))
    (else (build (first tora) (align (second tora)))))))

(define length* (lambda (tora)
  (cond
    ((atom? tora) 1)
    (else (o+ (length* (first tora)) (length* (second tora)))))))

(define weight* (lambda (tora)
  (cond
    ((atom? tora) 1)
    (else (o+
      (o* 2 (length* (first tora)))
      (length* (second tora)))))))

; dang, has non-total cases
(define shuffle (lambda (tora)
  (cond
    ((atom? tora) tora)
    ((a-pair? (first tora)) (shuffle (revpair tora)))
    (else (build (shuffle (first tora)) (shuffle (second tora)))))))

; lothar collatz
(define C (lambda (n)
  (cond
    ((one? n) 1)
    ((even? n) (C (o/ n 2)))
    (else (C (add1 (o* 3 n)))))))

; wilhelm ackerman
(define A (lambda (n m)
  (cond
    ((zero? n) (add1 m))
    ((zero? m) (A (sub1 n) 1))
    (else (A (sub1 n) (A n (sub1 m)))))))

