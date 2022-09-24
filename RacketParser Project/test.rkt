#lang racket
(require 2htdp/batch-io)


; Simple function to print every element in the list
; Just for testing purposes durring live running
(define (print-list lst)
  (cond [(null? lst) '()]
        [else (display (car lst))
              (newline)
              (print-list (cdr lst))]))





(define (remove-whitespace tokens)
    (filter (lambda (x) (not (member x '(" " "")))) tokens))



; Scans through list of white space removed tokens
; Calls scan-helper function to find and assign correct token to put in new list
; Puts token from scan-helper into new list that is returned
; Can be updated to only go through one at a time
(define (scan tokens)
    (cond
      [(null? tokens) '()]
      [else
        (cons (scan-helper (car tokens))
            (scan (cdr tokens)))]))
; From Scan function
; Checks to see if string being read in is any of the following knowns
; If it is then return correct token
; If not then check if it's a number
; If it is a number return number
; If it is not a number then return id
(define (scan-helper token)
    (cond
    [(string=? token "read") 'read]
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

(define (parse input)
  (define tokens (tokenize (read-1strings input)))
  (define tokens-no-whitespace (remove-whitespace tokens))
  (define tokens-scanned (scan tokens-no-whitespace))
  (display (remove-whitespace (tokenize (read-1strings input))))
  (display "\n")
  tokens-scanned)




(define (tokenize input)
  (define (tokenize-helper input current tokens)
    (cond [(null? input) (reverse tokens)]
          [(string=? (car input) " ") (tokenize-helper (cdr input) "" (cons current tokens))]
          [(member (car input) '("\n" "+" "-" "*" "/" "(" ")" )) (tokenize-helper (cdr input) "" (cons (car input) (cons current tokens)))]
          [else (tokenize-helper (cdr input) (string-append current (car input)) tokens)]))
  (tokenize-helper input "" '()))

;; add $$ to end of list
(define (add-end input)
  (append input '("$$")))