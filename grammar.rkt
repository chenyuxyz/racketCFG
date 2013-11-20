#lang racket

(provide (all-defined-out))

(struct term (str) #:transparent) ;; terminal
(struct var (char) #:transparent) ;; variable
(struct eps () #:transparent) ;; epsilon, use 'e in display
(struct rule (var to) #:transparent)

;; return true if r is a pair or null
(define (list0? r)
  (or (list? r) (null? r)))

;; concatenate tow pairs
(define (concat p1 p2)
  (define (_concat p1 p2)
    (if (null? p1) p2
        (_concat (cdr p1) (cons (car p1) p2))))
  (cond [(not (list0? p1)) (error (format "concat first argument is non-list: ~v" p1))]
        [(not (list0? p2)) (error (format "concat second argument is non-list: ~v" p2))]
        [else (_concat (_concat p1 null) p2)]))

;; display objects for easy reading
(define (show obj)
  (cond [(var? obj) (var-char obj)]
        [(term? obj) (term-str obj)]
        [(null? obj) ""]
        [(eps? obj) "'e"]
        [(list? obj) (string-append (show (car obj)) (show (cdr obj)))]
        [(rule? obj) (string-append (show (rule-var obj)) " -> " (show (rule-to obj)))]
        [else (error (format "show not support: ~v" obj))]))

;; given a pair and a rule, apply the rule to the left-most variable
(define (lstep st r)
  (cond [(not (list0? st)) (error "lstep non-list")]
        [(not (rule? r)) (error "lstep non-rule")]
        [(null? st) null]
        [(or (term? (car st)) (not (eq? (var-char (car st)) (var-char (rule-var r)))))
         (cons (car st) (lstep(cdr st) r))]
        [else (concat (rule-to r) (cdr st))]))