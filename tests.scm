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

(define xs-five-nested (list
	(list 'one 'one-point-five) (list 'two) (list 'three) (list 'four) (list 'five)))

(define food
  (list 'chips 'and 'fish 'or 'fish 'and 'fried))

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

(title "The Third Commandment")
(body "When building a list, describe the first typical")
(body "element, and then cons it onto the natural recursion.")
(insertR 'three-point-five 'three xs-five)
(insertL 'two-point-five 'three xs-five)
(subst 'three 'better-three xs-five)
(subst2 'four 'three 'five xs-five)
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

(title "The First Commandment")
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

(makeset food)
(set? (makeset food))
