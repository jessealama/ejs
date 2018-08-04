#lang br/quicklang

(provide port->ejsexpr
         port->ejsexprs
         string->ejsexpr
         ejsexpr->string
         ejsexpr->bytes)

(require (file "parse.rkt")
         (file "render.rkt"))

(module reader br
  (require (file "reader.rkt"))
  (provide read-syntax))
