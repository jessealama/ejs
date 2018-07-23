#lang racket/base

(provide parse-port)

(require racket/contract
         racket/port
         brag/support
         (file "grammar.rkt")
         (file "tokenizer.rkt"))

(define/contract (file-contents path)
  (path-string? . -> . bytes?)
  (define p (open-input-file path))
  (begin0
      (port->bytes p)
    (close-input-port p)))

(define/contract (parse-port port)
  (input-port? . -> . list?)
  (parse-to-datum
   (apply-tokenizer-maker make-tokenizer port)))

(define/contract (parse-file path)
  (path-string? . -> . list?)
  (define p (open-input-file path))
  (begin0
      (parse-port p)
    (close-input-port p)))

(module+ main
  (define p "test.json")
  (unless (file-exists? p)
    (displayln (format "No such file: ~a" p))
    (exit 1))
  (parse-file p))
