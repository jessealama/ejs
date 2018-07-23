EJS: Exact JSON for Racket
==========

EJS is an *exact JSON* library for Racket. It is a no-compromises—no numeric rounding—implementation of JSON for Racket.

EJS does essentially the same thing that Racket's standard JSON library, `json`, does. The main difference is that EJS's concept of numbers is *exact*: every number that EJS handles is an exact (read, rational) number. Given a a number like

    1.00000000000000000000000000001

EJS will handle this like the respectable rational number it is. Internally, such a thing will get handled as the lovely rational number

    1000000000000000000000000000001/1000000000000000000000000000000

rather than bowing in the face of practicality and using

    1.0

It's all about numbers. (EJS has nothing special to say about the non-numeric parts of JSON. They are handled just like the `json` package.)

# What's the difference between EJS and Racket's built-in JSON support? #

EJS is intended to be *just like Racket's built-in JSON library* with one exception: all numbers are exact.

In the EJS world, all numbers are rational (hence exact). In Racket's JSON world, inexact numbers are allowed.

This one difference—the insistence that all numbers are rational—shows up in three places:

## jsexpr? vs. ejsexpr? ##

Let's keep it short and sweet:

    > (jsexpr? #e3/5)
	#f

whereas

    > (ejsexpr? #e3/5)
	#t

## Parsing (bytes->jsexpr & string->jsexpr) ##

Given the difference above between `jsexpr?` and `ejsexpr?`, it is understandable that there would be a difference in parsing JSON. Thus:

    (string->jsexpr "1.0000000000000000000000000000000001")
	1.0

Compare:

    (string->ejsexpr "1.0000000000000000000000000000000001")
	10000000000000000000000000000000001/10000000000000000000000000000000000

EJS does not deal with a wider range of JSON inputs. Thus, since rational numbers are not part of the syntax of JSON, EJS rejects them. It takes a harsher stance on such inputs.

    (string->jsexpr "3/5")
	3

Versus

    (string->ejsexpr "3/5")
	error!

## Rendering (jsexpr->bytes & jsexpr->string) ##

EJS does not extend the official language of JSON. Thus, when rendering an ejsexpr? value, you get valid JSON.

In fact, you should find that `ejsexpr->string` and `string->ejsexpr` are inverses of each other: for all JSON-parseable strings `X`,

    (ejsexpr->string (string->ejsexpr X)) = X

holds, and, for all EJS expressions `E`,

    (string->ejsexpr (ejsexpr->string E)) = E

Such roundtripping is not guaranteed by Racket's built-in JSON package because it lowers the precision of some "high-precision" numbers.

## License ##

EJS is offered under the terms of the ISC License. See `LICENSE.txt`.
