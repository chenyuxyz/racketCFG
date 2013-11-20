#lang racket

(require "grammar.rkt")

(define p1 (pair (term "01") (end)))
(define p2 (pair (term "0") (pair (var "S") (pair (term "1") (end)))))
(define p3 (pair (term "0") (pair (var "R") (pair (term "1") (end)))))
(define r1 (rule (var "S") p1))
(define r2 (rule (var "S") p2))
(define ra1 (rule (var "S") (pair (eps) (end))))

(display "show pairs: ")
(show p2)
(display "show rules: ")
(show r2)
(display "apply the rule to the pair: ")
(show (lstep p2 r2))
