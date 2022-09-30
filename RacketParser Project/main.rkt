#lang racket
(require 2htdp/batch-io)
(require "scanner.rkt")



(define (match tokens token)
  (if (equal? (first tokens) token)
      (rest tokens)
      (display "Error in MATCH")))
  



(define (program tokens)
(case (first tokens)
                    [(or "id" "read" "write" "$$") (begin
                                                         (if
                                                      (match (stmt_list tokens) "end")
                                                      (display "program complete")
                                                      (display "Syntax Errors line 20")
                                                      ))]

  
                    [else (display "error line 22")]
                    )
  )


(define (stmt_list tokens)
  (case (first tokens)
    [(or "id" "read" "write") (begin
                                (stmt_list (stmt tokens)
                                           )
                                )
                              ]
    [(or "end" "E") tokens]
    )  
  )

(define (stmt tokens)
  (case (first tokens)
    [("id") (begin
              (expr(match (match tokens "id") "assign_op")))
            ]
    [("read") (match(match tokens "read") "id")]
    [("write") (expr(match tokens "write"))]
    )
  )

(define (expr tokens)
  (case (first tokens)
    [(or "id" "number" "l_paren") (begin
                                    (term_tail (term tokens))
                                    )]
    [(else) (display "error 54")]
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
    [(else) (display "error 66")])
                                       
 )
 

(define (term tokens)
  (case (first tokens)
    [(or "id" "number" "l_paren") (begin
                                    (factor_tail(factor tokens))
                                    )]
    [(else) (display "error 76")]
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
    [(else) (display "Error 88")]
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
    [(else) (display "Error 106")]
    ))



(define (add_op tokens)
  (case (first tokens)
    [("add_op") (begin
                  (match tokens "add_op"))
                ]
    [(else) (display "error 116")]
    )
  )


(define (mul_op tokens)
  (case (first tokens)
    [("mult_op") (begin
                   (match tokens "mult_op"))
                 ]
    [(else) (display "error 125")]
    )
  )




(define (parse input)
  (define tokens (list->string(scan(remove-whitespace(tokenize (read-1strings input))))))
  
  (program tokens)
  )

(parse "input01.txt")



