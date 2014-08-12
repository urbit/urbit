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


####`++scanf`

Scan with `;"`-interpolated parsers

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
