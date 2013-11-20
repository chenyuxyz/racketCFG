#lang racket

(require "grammar.rkt")

(define p1 (list (term "01")))
(define p2 (list (term "0") (var "S") (term "1")))
(define p3 (list (term "0") (var "R") (term "1")))
(define r1 (rule (var "S") p1))
(define r2 (rule (var "S") p2))
(define ra1 (rule (var "S") (list (eps))))

(display "show pairs: ")
(show p2)
(display "show rules: ")
(show r2)
(display "apply the rule to the pair: ")
(show (lstep p2 r2))
