#lang brag

documents: document*

document: boolean | null | string | number | object | array

boolean: TRUE | FALSE

string: DOUBLE-QUOTED-STRING

null: NULL

number: DIGITS | DECIMAL-DIGITS | NEGATIVE-DIGITS | NEGATIVE-DECIMAL-DIGITS

object : "{" [ object-item ("," object-item)* ] "}"

object-item : DOUBLE-QUOTED-STRING ":" document

array : "[" [ document ("," document )* ] "]"
