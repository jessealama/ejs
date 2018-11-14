#lang racket/base

(provide ejsexpr->bytes
         ejsexpr->string)

(require racket/contract
         racket/string
         racket/match
         racket/pretty
         (only-in racket/list
                  empty?
                  first
                  rest)
         (only-in racket/port
                  with-output-to-string)
         (file "./value.rkt"))

(module+ test
  (require rackunit))

(define/contract current-indent
  parameter?
  (make-parameter 0))

(define/contract (ejsexpr->bytes expr)
  (ejsexpr? . -> . bytes?)
  (string->bytes/utf-8 (ejsexpr->string expr)))

(define/contract (render-array-items items)
  ((listof ejsexpr?) . -> . void)
  (unless (empty? items)
    (cond [(empty? (rest items))
           (display (format "~a~a"
                              (pad)
                              (ejsexpr->string (first items))))]
          [else
           (displayln (format "~a~a,"
                              (pad)
                              (ejsexpr->string (first items))))
           (render-array-items (rest items))])))

(define/contract (render-object-item property value)
  (symbol? ejsexpr? . -> . string?)
  (format "\"~a\": ~a"
          (symbol->string property)
          (ejsexpr->string value)))

(define/contract (pad)
  (->* () () string?)
  (make-string (* 2 (current-indent)) #\space))

(define/contract (render-object-items items)
  ((listof (cons/c symbol? ejsexpr?)) . -> . void)
  (unless (empty? items)
    (match items
      [(list (cons (? symbol? property) value))
       (display (format "~a~a"
                        (pad)
                        (render-object-item property value)))]
      [(cons (cons (? symbol? property) value) more-items)
       (displayln (format "~a~a,"
                          (pad)
                          (render-object-item property value)))
       (render-object-items (rest items))])))

(define/contract (ejsexpr->string expr)
  (ejsexpr? . -> . non-empty-string?)
  (match expr
    ['null
     "null"]
    [#t
     "true"]
    [#f
     "false"]
    [(? number?)
     (define s
       (parameterize ([pretty-print-exact-as-decimal #t])
         (pretty-format expr)))
     (when (string-contains? s "/")
       (error (format "Number has no finite decimal representation: ~a"
                      expr)))
     s]
    [(? string?)
     (format "\"~a\"" expr)]
    [(? list?)
     (cond [(empty? expr)
            "[]"]
           [else
            (with-output-to-string
              (lambda ()
                (displayln "[")
                (parameterize ([current-indent (add1 (current-indent))])
                  (render-array-items expr))
                (newline)
                (display (format "~a]" (pad)))))])]
    [(? hash?)
     (cond [(hash-empty? expr)
            "{}"]
           [else
            (with-output-to-string
              (lambda ()
                (displayln "{")
                (parameterize ([current-indent (add1 (current-indent))])
                  (render-object-items (hash->list expr)))
                (newline)
                (display (format "~a}" (pad)))))])]
    [else
     (error (format "Don't know how to render ~a" expr))]))

(module+ test
  (check-equal? "null"
                (ejsexpr->string 'null))
  (check-equal? "true"
                (ejsexpr->string #t))
  (check-equal? "false"
                (ejsexpr->string #f))
  (check-equal? "3.14"
                (ejsexpr->string #e3.14))
  (check-equal? "[]"
                (ejsexpr->string (list)))
  (check-equal? "{}"
                (ejsexpr->string (hasheq)))
  (check-equal? "1.00000000000000000000000000000000001"
                (ejsexpr->string #e1.00000000000000000000000000000000001)))

(module+ test
  (define array-result #<<HI
[
  "hi",
  "bye"
]
HI
))
(module+ test
  (define object-result #<<OBJ
{
  "hi": "there",
  "bye": [
    null
  ]
}
OBJ
))
(module+ test
  (define object-result/reverse #<<OBJ
{
  "bye": [
    null
  ],
  "hi": "there"
}
OBJ
))
(module+ test
  (check-equal? (ejsexpr->string (list "hi" "bye"))
                array-result)
  (define object-computed (ejsexpr->string (hasheq 'hi "there" 'bye (list 'null))))
  (check-true (list? (member object-computed
                             (list object-result object-result/reverse)))
              object-computed))
