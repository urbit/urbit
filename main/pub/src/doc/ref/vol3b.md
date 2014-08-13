##section 3b, standard library

###section 3bD, JSON and XML

####`++jo`, json reparser

Contains converters of ++json to well-typed structures.

```
  =>  |%  ++  grub  (unit ,*) 
          ++  fist  $+(json grub)
      --
```

A `fist` is a gate that produces some manner of unit from json. Most arms in
`++jo` are fists, or produce them.

#####`++ar`

Parse JSON array as typed list.

```
~zod/try=> :type; ((ar ni):jo a/~[n/'1' n/'2'])
[~ u=~[1 2]]
{[%~ u=it(@)] %~}
```

#####`++at`

Parse JSON array as a fixed-length tuple.

```
~zod/try=> :type; ((ar ni):jo a/~[n/'3' s/'to' n/''])
[~ u=[3 'to' 4]]
{[%~ u=[@ @ta @]] %~}
```

#####`++bo`

    |=(jon=json ?.(?=([%b *] jon) ~ [~ u=p.jon]))

Parse JSON boolean.

#####`++bo`

    |=(jon=json ?.(?=([%b *] jon) ~ [~ u=!p.jon]))

Parse inverse of JSON boolean.

XX  Finish


####`++scanf`, formatted scanner

Scan with `;"`-interpolated parsers.

```
~zod/try=> `[p=@ud q=@ud]`(scanf "Score is 5 to 2" [;"Score is {n} to {n}"]:n=dim:ag)
[p=5 q=2]
```

```
~zod/try=> =n ;~(pfix (star (just '0')) (cook |=(@ud +<) dim:ag))
~zod/try=> (scanf "2014-08-12T23:10:58.931Z" ;"{n}\-{n}\-{n}T{n}:{n}:{n}.{n}Z")
[2.014 8 12 23 10 58 931]
~zod/try=> =dat (scanf "2014-08-12T23:10:58.931Z" ;"{n}\-{n}\-{n}T{n}:{n}:{n}.{n}Z")
~zod/try=> `@da`(year `date`dat(- [%& -.dat], |6 ~[(div (mul |6.dat (bex 16)) 1.000)]))
~2014.8.12..23.10.58..ee56
```

####`++parsf`, `;"`-interpolated parser generator

`parsf` generates a `_rule` from a tape with rules embedded in it, literal
sections being matched verbatim. The parsed type is a tuple of the embedded
rules' results.

Two intermediate arms are used:

#####`++norm`

```
::  .=  (norm [;"{n}, {n}"]:n=dim:ag)  ~[[& dim] [| ", "] [& dim]]:ag
  ++  norm                                             
    |*  (pole ,_:/(*$&(_rule tape)))
    ?~  +<  ~
    =>  .(+< [i=+<- t=+<+])
    :_  t=$(+< t)
    =+  rul=->->.i
    ^=  i
    ?~  rul     [%| p=rul]
    ?~  +.rul   [%| p=rul]
    ?@  &2.rul  [%| p=;;(tape rul)]
    [%& p=rul]
```

`norm` converts a `;"` pole of `[[%~. [%~. ?(tape _rule)] ~] ~]` into a more
convenient list of discriminated tapes and rules.

#####`++bill`

```
  ::  .=  (bill ~[[& dim] [| ", "] [& dim]]:ag)
  ::  ;~(plug dim ;~(pfix com ace ;~(plug dim (easy)))):ag
  ++  bill
    |*  (list (each ,_rule tape))
    ?~  +<  (easy ~)
    ?:  ?=(| -.i)  ;~(pfix (jest (crip p.i)) $(+< t))
    %+  cook  |*([* *] [i t]=+<)
    ;~(plug p.i $(+< t))
  --
```

`bill` builds a parser out of rules and tapes, ignoring the literal sections
and producing a list of the rules' results.




