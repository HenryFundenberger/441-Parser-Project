#lang racket
(require 2htdp/batch-io)
(require "scanner.rkt")



(define (match tokens token)
  (if (equal? (first tokens) token)
      (rest tokens)
      (rest tokens)))
  



(define  (program tokens)
                  (case (first tokens)
                    [(or "id" "read" "write" "end") (begin
                                                     (if (equal? (match (stmt_list tokens) "end") '())
                                                         (display "Accept")
                                                         (display "Syntax Error!")
                                                     ))]
                    [else (display "error")]
                    )
)
  


(define (stmt_list tokens)
  (case (first tokens)
    [(or "id" "read" "write") (begin
                                (stmt_list (stmt tokens)))]
    [("E") (begin (stmt_list(match tokens "E")))]
    [("end") tokens]
    [else tokens]
    )  
  )

(define (stmt tokens)
  (case (first tokens)
    [("id") (begin
              (expr(match (match tokens "id") "assign_op")))]
    [("read") (match(match tokens "read") "id")]
    [("write") (expr(match tokens "write"))]
    [else tokens]
    )
  )

(define (expr tokens)
  (case (first tokens)
    [(or "id" "number" "l_paren") (begin
                                    (term_tail (term tokens))
                                    )]
    [(else) (display "error 52")]
    )
  )

(define (term_tail tokens)
  (case (first tokens)
    [(or "add_op") (begin
                     (term_tail(term(add_op tokens)))
                     )]
    [(or "r_paren" "id" "write" "end" "E")(begin
                                        tokens)
                                      ]
    [else tokens]
    )
                                       
 )
 

(define (term tokens)
  (case (first tokens)
    [(or "id" "number" "l_paren") (begin
                                    (factor_tail(factor tokens))
                                    )]
    [("E") (begin factor_tail(factor(match tokens "E")))]
    [else tokens]
    )
  )

(define (factor_tail tokens)
  (case (first tokens)
    [(or "mul_op") (begin
                     (factor_tail(factor(mul_op tokens)))
                     )]
    [(or "add_op" "r_paren" "id" "write" "end" "E") (begin
                                                  tokens)
                                                ]
    [else tokens]
    )
  )                             
 
 

(define (factor tokens)
  (case (first tokens)
    [("id") (begin
              (match tokens "id")
              )]
    [("number") (begin
                  (match tokens "number")
                  )]
    [("l_paren") (begin
                   (match (expr(match tokens "l_paren")) "r_paren")
                     )]
    [("E") (begin (match tokens "E"))]
    [else tokens]
    ))



(define (add_op tokens)
  (case (first tokens)
    [("add_op") (begin
                  (match tokens "add_op"))
                ]
    [else tokens]
    )
  )


(define (mul_op tokens)
  (case (first tokens)
    [("mul_op") (begin
                   (match tokens "mul_op"))
                 ]
    [else tokens]
    )
  )




(define (parse input)
  (define tokens (list->string(scan(remove-whitespace(tokenize (read-1strings input))))))
  
  (program tokens)
  )

(parse "input05.txt")



