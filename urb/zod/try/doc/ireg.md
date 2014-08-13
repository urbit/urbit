Some obscure `:-` irregular forms.
==================================

Moveme: irregular form doc

###Infix `^`

```
~zod/main=/app> 1^2^3
[1 2 3]
```

`a^b` is equivalent to `:-  a  b`

###Infix `/`

```
~zod/main=/app> a/1
[%a 1]
~zod/main=/app> a/'twig'
[%a 'twig']
```

Like `^`, but first item must be a term, and is cubed. Used to construct paths
and fronds.

###Prefix `` ` ``, postfix `~`

```
~zod/main=/app> ````20
[~ ~ ~ ~ 20]
~zod/main=/app> [42 30]~
[[42 30] ~]
```

Complimenting each other, these construct pairs with nil in the head and tail
respectively. Multiple postfix `~` do not work for unknown reasons.
