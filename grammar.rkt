#lang racket

(provide (all-defined-out))

(struct term (str) #:transparent) ;; terminal
(struct var (char) #:transparent) ;; variable
(struct eps () #:transparent) ;; epsilon, use 'e in display
(struct pair (e1 e2) #:transparent)
(struct end () #:transparent)
(struct rule (var to) #:transparent)

;; return true if r is a pair or an end
(define (pair0? r)
  (or (pair? r) (end? r)))

;; concatenate tow pairs
(define (concat p1 p2)
  (define (_concat p1 p2)
    (if (end? p1) p2
        (_concat (pair-e2 p1) (pair (pair-e1 p1) p2))))
  (cond [(not (pair0? p1)) (error (format "concat first argument is non-pair: ~v" p1))]
        [(not (pair0? p2)) (error (format "concat second argument is non-pair: ~v" p2))]
        [else (_concat (_concat p1 (end)) p2)]))

;; display objects for easy reading
(define (show obj)
  (cond [(var? obj) (var-char obj)]
        [(term? obj) (term-str obj)]
        [(end? obj) ""]
        [(eps? obj) "'e"]
        [(pair? obj) (string-append (show (pair-e1 obj)) (show (pair-e2 obj)))]
        [(rule? obj) (string-append (show (rule-var obj)) " -> " (show (rule-to obj)))]
        [else (error (format "show not support: ~v" obj))]))

;; given a pair and a rule, apply the rule to the left-most variable
(define (lstep st r)
  (cond [(not (pair0? st)) (error "lstep non-pair")]
        [(not (rule? r)) (error "lstep non-rule")]
        [(end? st) (end)]
        [(or (term? (pair-e1 st)) (not (eq? (var-char (pair-e1 st)) (var-char (rule-var r)))))
         (pair(pair-e1 st) (lstep(pair-e2 st) r))]
        [else (concat (rule-to r) (pair-e2 st))]))