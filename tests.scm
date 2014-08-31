#lang scheme

(require "schemer.scm")

; micro unit testing -____-
(define (assert msg test)
  (if (not test) (error msg) (displayln (string-append "ok: " msg))))

; print helpers
(define (msg before message after) (displayln
  (string-append before message after)))

(define (block b) (msg "-- " b " --"))
(define (title t) (displayln "") (msg "** " t " **"))
(define (body b) (msg "   " b ""))

; sample data
(define xs-null '())
(define xs-five (list 'one 'two 'three 'four 'five))
(define xs-five-mix (list 'one 1 'two 2 3 4 'three 'four 5 'five))
(define xs-five-nested
  (list
    (list 'one 'one-point-five)
    (list 'two)
    (list 'three)
    (list 'four)
    (list 'five)))

(define food
  (list 'chips 'fish 'cream 'fish 'potato 'hummus 'leek 'chips))

(define tup-one (list 1 2))
(define tup-two (list (list 1 2) (list 3 4)))

; atom?
(assert "is atom?" (atom? 'test-atom))

; quick list test
(block "List test")
(title "The law of car")
(body "The primitive car is defined")
(body "only for non-empty lists.")
(car xs-five)

(title "The law of cdr")
(body "The primitive cdr is defined only for")
(body "not-empty lists. The cdr of any non-empty")
(body "list is always another list.")
(cdr xs-five)
(car (cdr (cdr xs-five)))

(title "The law of cons")
(body "The primitive cons takes two arguments.")
(body "The second argument to cons must be a list.")
(body "The result is a list.")
(cons 'first xs-five)
(cons 'firster xs-five)
(cons 'one (car xs-five))
(cons 'new-one (cdr xs-five))

(title "The law of null?")
(body "The primitive null? is defined only for lists.")
(null? xs-null)
(null? xs-five)

(title "The law of eq?")
(body "The primitive eq? takes two arguments.")
(body "Each must be a non-numeric atom.")
(eq? 'two 'two)
(eq? 'two 'three)

(title "The First Commandment (preliminary)")
(body "Always ask null? as the first question")
(body "in expressing any function.")
(rember 'two xs-five)
(rember 'two xs-five-nested)

(title "The Second Commandment")
(body "Use cons to build lists.")
(firsts xs-five-nested)
(seconds xs-five-nested)

(title "The Third Commandment")
(body "When building a list, describe the first typical")
(body "element, and then cons it onto the natural recursion.")
(insertR 'three-point-five 'three xs-five)
(insertL 'two-point-five 'three xs-five)
(subst 'three 'better-three xs-five)
(subst2 'six 'five 'four xs-five)
(multirember 'four (insertL 'four 'four xs-five))
(multiinsertR 'tomato 'fish food)
(multiinsertL 'fried 'fish food)

(title "The Fourth Commandment")
(body "Always change at least one argument while recurring.")
(body "It must be changed to be closer to termination.")
(body "The changing argument must be tested in the termination")
(body "condition: when using cdr, test termination with null?.")
(multisubst 'crabs 'fish food)

(title "Numbers")
(add1 67)
(sub1 5)
(sub1 0)
(zero? 0)
(zero? 1492)

(o+ 4 4)
(o- 17 7)

(title "Tuples")
;;(tup? (1 32 312))
(addtup '(12 12 12))

(o* 5 5)
;; (x 5 5)  = 5 + (x 5 4)
;;      = 5 + 5 + (x 5 3)
;;      = 5 + 5 + 5 + (x 5 2)
;;      = 5 + 5 + 5 + 5 + (x 5 1)
;;      = 5 + 5 + 5 + 5 + 5 + (x 5 0)
;;      = 5 + 5 + 5 + 5 + 5 + 0

(tup+ '(2 2) '(4 4 4))

(> 12 133)
(o> 134 133)
(o< 6 5)
(o=? 4 4)
(o= 4 4)
(o** 5 3)

(o/ 15 4)
;; (/ 15 4) = 1 + (/ 11 4)
;;      = 1 + 1 + (/ 7 4)
;;      = 1 + 1 + 1 (/ 3 4)
;;      = 1 + 1 + 1 + 0

(length xs-five)

(pick 1 '(1 2 3 4 5 6))
(pick 1 xs-five)
(rempick 2 '(1 2 3))
(rempick 2 xs-five)

(no-nums xs-five-mix)
(all-nums xs-five-mix)

(eqan? 2 2)
(eqan? 2 'two)
(eqan? 'two 'two)

(occur 2 '(1 2 3 4 5 2 2))
(one? 1)
(rempick2 2 '(one two three four))

(rember* 2 '(2 3 (4 5) 6 8 9 (2 1 2 (2 3 3))))
(insertR* 'sir 'yes '(oh yes (oh yes (oh yes) oh yes)))
(occur* 2 '(2 2 2 (2 3 4 (43 2))))
(subst* 'yesterday 'today '(today was a nice day (yes today)))
(insertL* 'before 'today '(today was a nice day (yes today)))
(member* 'yes '(today was a nice day (yes today)))
(leftmost '(((maybe) try this for a change (today) please)))

(eqlist? '(a b (aa)) '(a b (aa)))
(equal? '(a) '(ab))

(eqlist?r '(a b (aa)) '(a b (aa)))
(remberr 'test '(this is a big test))


(title "The First Commandment (final version)")
(body "When recurring on a list of atoms, lat, ask two questions")
(body "about it: (null? lat) and else.")
(body "When recurring on a number, n, ask two questions about")
(body "it: (zero? n) and else.")

(numbered? '(2 o+ 2))

(title "The Seventh Commandment")
(body "Recur on the subparts that are of the same nature")
(body "- On the sublists of a list.")
(body "- On the subexpressions of an artihmetic expressions.")

(1st-sub-exp '('one 'two 'three))
(2nd-sub-exp '('one 'two 'three))

(value '(o+ 4 4))
(value '(o* 4 4))
(value '(o** 4 4))

(title "The Eighth Commandment")
(body "Use help functions to abstract from representations.")

(set? xs-five)
(set? food)
(set? xs-five-mix)

(makeset-old food)
(set? (makeset food))

(makeset food)
(subset? '(chips fish) (makeset food))
(eqset? (makeset food) (makeset food))
(intersect? (makeset food) food)
(intersect (makeset food) food)
(intersect '(seven eight one five) (makeset xs-five))
(union '(seven eight one five) xs-five)
(xxx '(seven eight one five) xs-five)
(intersect-all '((one two) (seven one three)))

(a-pair? tup-one)
(a-pair? '(1 2 3))
(a-pair? tup-two)

(first tup-two)
(second tup-two)

(build xs-five food)
(fun? tup-two)

(revrel tup-two)

(fullfun? tup-two)
(one-to-one? tup-two)

(firsts tup-two)
(seconds tup-two)

((rember-f eq?) 'one xs-five)
((insertL-f eq?) 'eleventy 'one xs-five)
((insertR-f eq?) 'eleventy 'one xs-five)

(insertLr 'eleventy 'one xs-five)
(insertRr 'eleventy 'one xs-five)

(insertLrr 'smementy 'one xs-five)
(subst_r 'smementy 'one xs-five)

(title "The Ninth Commandment")
(body "Abstract common patterns with a new function")

((atom-to-function 'o*) 2 2)
(value_r '(o+ 4 4))
(value_r '(o* 4 4))
(value_r '(o** 4 4))

((multirember_r eq?) 'fish food)
(multirember-eq? 'fish food)
(multiremberT (lambda (a) (eq? a 'fish)) food)

(multirember&co 'food food a-friend)
(multirember&co 'food food last-friend)

(title "The Tenth Commandment")
(body "Build functions to collect more than one value at a time")
