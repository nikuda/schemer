#lang scheme

(require "schemer.scm")

; micro unit testing -____-
(define (assert msg test)
	(if (not test) (error msg) (displayln (string-append "ok: " msg))))

(assert "is atom?" (atom? 'test-atom))

