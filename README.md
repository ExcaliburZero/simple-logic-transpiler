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
