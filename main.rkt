#lang racket/base

(require racket/contract
         racket/function
         racket/match
         racket/port
         racket/list
         (file "value.rkt")
         (file "parser.rkt"))

(module+ test
  (require rackunit))

(define digits
  (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define (digit-char? x)
  (and (char? x)
       (list? (member x digits char=?))))

(define/contract (parse/digits port digits)
  (input-port? (listof digit-char?) . -> . (listof char?))
  (define char (peek-char/error port))
  (cond [(char? char)
         (match char
           [(? digit-char?)
            (parse/digits port
                          (cons (read-char port)
                                digits))]
           [else
            (reverse digits)])]
        [(eof-object? char)
         (reverse digits)]
        [#f
         (error "Invalid UTF-8 byte sequence encountered!")]))

(define/contract (parse/non-negative-number port before-dot-digits)
  (input-port? (listof digit-char?) . -> . (and/c ejs-number?
                                                  (not/c negative?)))
  (define digits (parse/digits port before-dot-digits))
  (define char (peek-char/error port))
  (cond [(char? char)
         (match char
           [#\.
            (read-char port)
            (define after-decimal (parse/digits port (list)))
            (define formatted
              (format "~a.~a"
                      (list->string digits)
                      (list->string after-decimal)))
            (parameterize ([read-decimal-as-inexact #f])
              (string->number formatted))]
           [else
            (error (format "While trying to parse a negative number, we encountered an unexpected character \"~a\"."
                           char))])]
        [(eof-object? char)
         (parameterize ([read-decimal-as-inexact #f])
           (string->number (list->string digits)))]
        [#f
         (error "Invalid UTF-8 byte sequence encountered!")]))

(define/contract (parse/negative-number port)
  (input-port? . -> . (and/c ejs-number?
                             (not/c positive?)))
  (- (parse/non-negative-number port (list))))

(define/contract (parse/object port)
  (input-port? . -> . ejs-object?)
  (hash))

(define/contract (parse/expecting port previous-char expecting success-value)
  (input-port? (or/c false/c char?) (listof char?) ejsexpr? . -> . ejsexpr?)
  (cond [(empty? expecting)
         success-value]
        [else
         (define e (first expecting))
         (define c (read-char/error port))
         (cond [(eof-object? c)
                (cond ([char? previous-char]
                       (error "Unexpected end of file encountered (just read \"~a\")." previous-char))
                      (else
                       (error "Unexpected end of file encountered.")))]
               [(char? c)
                (unless (char=? c e)
                  (cond [(char? previous-char)
                         (error (format "Unexpected character encountered (read \"~a\" after \"~a\", expecting \"~a\"." c previous-char e))]
                        [else
                         (error (format "Unexpected character encountered (read \"~a\", expecting \"~a\"." c e))]))]
               [(eq? c #f)
                (cond [(char? previous-char)
                       (error "Invalid UTF-8 character encountered (last legal character read was \"~a\")." previous-char)]
                      [else
                       (error "Invalid UTF-8 character encountered.")])])
         (parse/expecting port
                          c
                          (rest expecting)
                          success-value)]))

(define/contract (parse/true port)
  (input-port? . -> . ejs-boolean?)
  (parse/expecting port #f (list #\r #\u #\e) #t))

(define/contract (parse/false port)
  (input-port? . -> . ejs-boolean?)
  (parse/expecting port #f (list #\a #\l #\s #\e) #f))

(define/contract (parse/null port)
  (input-port? . -> . ejs-null?)
  (parse/expecting port #f (list #\u #\l #\l) 'null))

(define/contract (peek-char/error port)
  (input-port? . -> . (or/c eof-object? char? false/c))
  (with-handlers ([exn:fail? (const #f)])
    (peek-char port)))

(define/contract (read-char/error port)
  (input-port? . -> . (or/c eof-object? char? false/c))
  (with-handlers ([exn:fail? (const #f)])
    (read-char port)))

(define/contract (parse/string port preceding-chars)
  (input-port? (listof char?) . -> . ejs-string?)
  (define c (read-char/error port))
  (cond [(eof-object? c)
         (error "End of input encountered while reading a string!")]
        [(eq? c #f)
         (error "Invalid UTF-8 byte sequence encountered while reading a string!")]
        [(char? c)
         (match c
           [#\"
            (list->string (reverse preceding-chars))]
           [else
            (parse/string port (cons c preceding-chars))])]))

(define whitespace-chars
  (list #\newline
        #\tab
        #\space))

(define (whitespace? x)
  (list? (member x whitespace-chars char=?)))

(define/contract (expand-ejs ejs)
  (list? . -> . ejsexpr?)
  (match ejs
    [(list 'document (list (or 'string 'null 'boolean) v))
     v]
    [(list 'document (list 'number num))
     (parameterize ([read-decimal-as-inexact #f])
       (string->number num))]
    [(list 'document (list 'array "[" item ... "]"))
     (map expand-ejs
          (remove "," item))]
    [(list 'document (list 'object "{" item ... "}"))
     (for/hasheq ([i (remove "," item)])
       (match i
         [(list 'object-item oi ":" val)
          (values (string->symbol oi)
                  (expand-ejs val))]))]
    [else
     (error (format "Cannot make sense of EJS parse tree: ~a" ejs))]))

(define/contract (parse-ejsexprs input)
  ((or/c input-port? string?) . -> . (listof ejsexpr?))
  (define to-evaluate
    (cond [(input-port? input)
           (parse-port input)]
          [(string? input)
           (call-with-input-string input parse-port)]))
  (match to-evaluate
    [(cons 'documents documents)
     (map expand-ejs documents)]
    [else
     (error (format "Cannot make sense of documents: ~a" to-evaluate))]))

(module+ test
  (check-equal? (list "hi there!")
                (parse-ejsexprs "\"hi there!\""))
  (check-equal? (list 'null)
                (parse-ejsexprs "null"))
  (check-equal? (list #t)
                (parse-ejsexprs "true"))
  (check-equal? (list #f)
                (parse-ejsexprs "false"))
  (check-equal? (list 4)
                (parse-ejsexprs "4"))
  (check-equal? (list #e4.5)
                (parse-ejsexprs "4.5"))
  (check-equal? (list -42)
                (parse-ejsexprs "-42"))
  (check-equal? (list #e-42.98)
                (parse-ejsexprs "-42.98"))
  (check-equal? (list (list))
                (parse-ejsexprs "[]"))
  (check-equal? (list (list 42))
                (parse-ejsexprs "[ 42 ]"))
  (check-equal? (list #e1.000000000000000000000001)
                (parse-ejsexprs "1.000000000000000000000001"))
  (check-equal? (list (hasheq 'hi "there"))
                (parse-ejsexprs "{\"hi\": \"there\"}"))
  (check-equal? (list (hasheq 'hi "what"))
                (parse-ejsexprs "{\"hi\": \"there\", \"hi\": \"what?\"}")))
