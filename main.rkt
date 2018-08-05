#lang br/quicklang

(provide port->ejsexpr
         port->ejsexprs
         string->ejsexpr
         bytes->ejsexpr
         ejsexpr->string
         ejsexpr->bytes
         ejsexpr?
         ejs-array?
         ejs-object?
         ejs-null?
         ejs-number?
         ejs-integer?
         ejs-string?
         ejs-boolean?
         equal-ejsexprs?)

(require (file "parse.rkt")
         (file "render.rkt")
         (file "value.rkt")
         (file "equal.rkt"))

(module reader br
  (require (file "reader.rkt"))
  (provide read-syntax))
