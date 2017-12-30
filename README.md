# simple-logic-transpiler
This is a transplier for translating logic expressions that use traditional proposition logic operators into expressions that only use the Sheffer Stroke.

```
$ stack exec simple-logic-transpiler-exe
(A * B)
((A | B) | (A | B))
```

## Compiling
```
$ stack build
```

## Translation Rules
* `A * B` => `((A | B) | (A | B))`
* `A v B` => `((A | A) | (B | B))`
* `A -> B` => `(A | (A | B))`
* `!A` => `(A | A)`

## Grammar
```
expression ::= proposition | unaryOperation | binaryOperation

proposition ::= uppercaseLetter

unaryOperation ::= negation

negation ::= "!" expression

binaryOperation ::= "(" expression " " binaryOperator " " expression ")"

binaryOperator ::= and | or | implies

and ::= "*"

or ::= "v"

implies ::= "->"
```

## Abstract Syntax Trees
### Source Expressions
```
data SExpr =
    And SExpr SExpr
  | Or SExpr SExpr
  | Implies SExpr SExpr
  | Not SExpr
  | SProp String
```

```
(A * !B)

And (SProp "A") (Not (SProp "B"))

       And
      /   \
     /     \
  SProp    Not
    |       |
    A     SProp
            |
            B
```

### Destination Expressions
```
data DExpr =
    Stroke DExpr DExpr
  | DProp String
```

```
(A | (A | B))

Stroke (DProp "A") (Stroke (DProp "A") (DProp "B"))

      Stroke
     /      \
    /        \
 DProp      Stroke
   |        /    \
   A       /      \
        DProp    DProp
          |        |
          A        B
```
