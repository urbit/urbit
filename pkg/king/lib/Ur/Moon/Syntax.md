# Moon Syntax

## Core

```
var  --  x
app  --  (x y)
lam  --  <x/* x>
nat  --  123

box  --  box has no type
typ  --  typ/box
nat  --  nat/typ
box  --  box/{* *}
fun  --  fun/{* * *}

inc  --  inc/{@ @}
fol  --  fol/{@ a/* a {a a} a}
box  --  box/{a/* a (box a)}
opn  --  opn/{a/* (box a) a}
fir  --  fir/{(box {a b}) a b}
evl  --  evl/{a/* @ a}
dum  --  dum/{a/* a @}
```


# Basic Sugar

```
(x y z) -> ((x y) z)
<x y z> -> <x <y z>>
<x y z> -> <x <y z>>
{x y z} -> {x {y z}}
```


# Runic Sugar

```
|=(x y)    ->  |=  x  y
%*(x y z)  ->  %*  x  y  z  ##
```

## Core Runes

```
(f x)        <-  %-  f  x
(f x y)      <-  %+  f  x  y
(f x y z)    <-  %@  f  x  y  z
(f x y)      <-  %*  f  ++  x  ##

{a b}        <-  $-  a  b
{a b c}      <-  $+  a  b  c
{a b c d}    <-  $@  a  b  c  d
{a b c}      <-  $*  a  ++  b  ++  c  ##

<x y>        <-  |=  x  y
<x y z>      <-  |=  (x y)  z

(<x/a b> v)  <-  =/  x/a  v  b
```


## Lexemes

```
'('     LIT
')'     RIT
'{'     LOB
'}'     ROB
'<'     LEF
'>'     RIT
'/'     FAS

/  +/   GAP
'\n'    GAP
' '     ACE

"=/"    LET
"|="    LAM
"%-"    AP2
"%+"    AP3
"%@"    AP4
"%*"    APN
"$-"    PI2
"$+"    PI3
"$@"    PI4
"$*"    PIN
"##"    END

/[*@~`!@#$%^&*-_+=\|;:><,.?]/  SYM(*)
[a-zA-Z][a-zA-Z0-9-]*          SYM(*)
[0-9]+                         NUM(*)
```
