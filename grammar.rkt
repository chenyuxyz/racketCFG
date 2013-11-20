#lang racket

(struct term (string) #:transparent)
(struct var (char) #:transparent)
(struct eps () #:transparent)
(struct pair (e1 e2) #:transparent)
(struct end () #:transparent)
(struct rule (start to) #:transparent)

(define p1 (pair (term "01") (end)))
(define p2 (pair (term "0") (pair (var "S") (pair (term "1") (end)))))
(define p3 (pair (term "0") (pair (var "R") (pair (term "1") (end)))))
(define r1 (rule (var "S") p1))
(define r2 (rule (var "S") p2))

(define ra1 (rule (var "S") (pair (eps) (end))))

(define (pair0? r)
  (or (pair? r) (end? r)))

(define (concat p1 p2)
  (define (_concat p1 p2)
    (cond [(or (not (pair0? p1)) (not (pair0? p2))) (error "concat applies to non-pair")]
          [(end? p1) p2]
          [else (_concat (pair-e2 p1) (pair (pair-e1 p1) p2))]))
  (_concat (_concat p1 (end)) p2))

(define (show obj)
  (cond [(var? obj) (var-char obj)]
        [(term? obj) (term-string obj)]
        [(end? obj) ""]
        [(eps? obj) "'e"]
        [(pair? obj) (string-append (show (pair-e1 obj)) (show (pair-e2 obj)))]
        [(rule? obj) (string-append (show (rule-start obj)) " -> " (show (rule-to obj)))]
        [else (error (format "show not support: ~v" obj))]))

(define (lstep st r)
  (cond [(not (pair0? st)) (error "lstep non-pair")]
        [(not (rule? r)) (error "lstep non-rule")]
        [(end? st) (end)]
        [(or (term? (pair-e1 st)) (not (eq? (var-char (pair-e1 st)) (var-char (rule-start r)))))
         (pair(pair-e1 st) (lstep(pair-e2 st) r))]
        [else (concat (rule-to r) (pair-e2 st))]))