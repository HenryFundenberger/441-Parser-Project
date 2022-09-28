#lang racket
(require 2htdp/batch-io)



(define (remove-whitespace tokens)
    (filter (lambda (x) (not (member x '(" " "")))) tokens))
(provide remove-whitespace)



(define (scan tokens)
    (cond
      [(null? tokens) '()]
      [else
        (cons (scan-helper (car tokens))
            (scan (cdr tokens)))]))
(provide scan)


(define (scan-helper token)
    (cond
    [(string=? token "read") read]
    [(string=? token "write") 'write]
    [(string=? token "+") 'add_op]
    [(string=? token "-") 'add_op]
    [(string=? token "*") 'mul_op]
    [(string=? token "/") 'mul_op]
    [(string=? token "(") 'l_paren]
    [(string=? token ")") 'r_paren]
    [(string=? token ":=") 'assign_op]
    [(string=? token "\n") 'E]
    [(string=? token "$$") 'end]
    [(number? (string->number token)) 'number]
    [else 'id]))
(provide scan-helper)



(define (tokenize input)
  (define (tokenize-helper input current tokens)
    (cond [(null? input)
    ; append final token
    (reverse (cons current tokens))]
          [(string=? (car input) " ") (tokenize-helper (cdr input) "" (cons current tokens))]
          [(member (car input) '("\n" "+" "-" "*" "/" "(" ")" "$$" )) (tokenize-helper (cdr input) "" (cons (car input) (cons current tokens)))]
          [(string=? (car input) " ") (tokenize-helper (cdr input) "" (cons current tokens))]
          [(member (car input) '("\n" "+" "-" "*" "/" "(" ")" "$$" )) (tokenize-helper (cdr input) "" (cons (car input) (cons current tokens)))]
          [else (tokenize-helper (cdr input) (string-append current (car input)) tokens)]))
  (tokenize-helper input "" '()))
(provide tokenize)


