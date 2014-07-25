!:
::  /=main=/bin/app/hoon
::
=>  %=    .
        +
      =>  +
      |%
++  down  (list bloc)                                   ::  markdown model
++  bloc                                                ::  block elements
  $%  [%head p=@ud q=(list span) r=(unit tape)]
      [%para p=(list span)]
      [%lise q=down] 
      [%list p=? q=down] 
      [%quot p=down]
      [%horz ~]
      [%code p=wall]
      [%html p=manx]
  ==
::
++  span                                                 ::  span elements
  $%  [%text p=tape] 
      [%emph p=term q=(list span)]
      [%stri p=(list span)]
      [%brek ~]
      [%link p=(list span) q=tape r=(unit tape)]
      [%cods p=tape]
  ==
::
++  apex                                                :: markdown parser
  |=  tub=nail
  ^-  (like down)
  =+  sep=(sepa tub)
  ?~  q.sep
    [p.sep ~]
  [p.sep (some [(turn p.u.q.sep |=(a=tape (scan a blos))) [p.sep ~]])]
::
++  blos                                                ::  block element
  (knee *bloc |.(~+(;~(pose head quot lasd horz code codf html para))))
::
++  brek  (stag %brek (cold ~ ;~(plug fas fas)))        ::  line break
++  chrd  ;~(pose escp prn (cold ' ' eol))              ::  span character data
++  code                                                ::  code block
  %+  stag  %code
  %-  plus
  ;~  pfix  (stun [4 4] ace)
    ;~(pose (cook welp ;~(plug (plus prn) (cold "\0a" eol))) (full (plus prn)))
  ==
::
++  codf                                                ::  fenced code block 
  %+  stag  %code
  %+  ifix  [;~(plug tec tec tec eol) ;~(plug tec tec tec)]
    %-  plus
      ;~  pose 
        (cook welp ;~(plug (plus prn) (cold "\0a" eol)))
        (full (plus ;~(less ;~(plug tec tec tec) prn)))
      ==
::
++  cods                                                ::  code span
  %+  stag  %text
  =+  chx=;~(pose (cold ' ' eol) prn)
  ;~  pose
      %+  ifix  [(jest '```') (jest '```')]  (plus ;~(less (jest '```') chx))  
      %+  ifix  [(jest '``') (jest '``')]    (plus ;~(less (jest '``') chx))
      %+  ifix  [tec tec]                    (plus ;~(less tec chx))
  ==
::
++  dont  ;~(pose tar tec cab ;~(plug sig sig) ;~(plug fas fas) sel)
++  spas                                                ::  all span elements
  |*  res=_rule
  (plus ;~(pose emph stri link brek cods (text res)))
::
++  eol  (just `@`10)                                   ::  newline
++  emph                                                ::  emphasis
  %+  knee  *span  |.  ~+
  %+  stag  %emph
  =+  inn=(plus ;~(pose cods stri link (text fail)))
  ;~  pose
    (ifix [(jest '***') (jest '***')] (stag %both inn))
    (ifix [(jest '**_') (jest '_**')] (stag %both inn))
    (ifix [(jest '*__') (jest '__*')] (stag %both inn))
    (ifix [(jest '_**') (jest '**_')] (stag %both inn))
    (ifix [(jest '__*') (jest '*__')] (stag %both inn))
    (ifix [(jest '___') (jest '___')] (stag %both inn))
    (ifix [(jest '**') (jest '**')] (stag %bold inn))
    (ifix [(jest '__') (jest '__')] (stag %bold inn))
    (ifix [tar tar] (stag %ital inn))
    (ifix [cab cab] (stag %ital inn))
  ==
::
++  escp                                                ::  escapable chars
  ;~  pose
    (cold '`' (jest '\\`'))
    (cold '*' (jest '\\*'))
    (cold '#' (jest '\\#'))
    (cold '-' (jest '\\-'))
    (cold '.' (jest '\\.'))
    (cold '{' (jest '\\{'))
    (cold '}' (jest '\\}'))
    (cold '[' (jest '\\['))
    (cold ']' (jest '\\]'))
    (cold '\\' (jest '\\\\'))
  ==
::
++  head                                                ::  header
  %+  stag  %head
  =+  ^=  hed
      ;~  pose
        ;~  plug 
          ;~(pfix wits (spas hax)) 
          (cook some (ifix [;~(plug (star hax) sel hax) ser] (plus alp)))
        ==
        (ifix [wits (star hax)] ;~(plug (spas hax) (easy ~)))
      ==
  =+  ^=  sed
      ;~  pose
        ;~  plug 
          (spas ;~(pose eol sel))
          (cook some (ifix [;~(plug sel hax) ser] (plus alp)))
        ==
        ;~(plug (spas eol) (easy ~))
      ==
  ;~  pose
    ;~  pfix  (jest '######')  (stag 6 hed)  ==
    ;~  pfix  (jest '#####')   (stag 5 hed)  ==
    ;~  pfix  (jest '####')    (stag 4 hed)  ==
    ;~  pfix  (jest '###')     (stag 3 hed)  ==
    ;~  pfix  (jest '##')      (stag 2 hed)  ==
    ;~  pfix  (jest '#')       (stag 1 hed)  ==
    (stag 1 (ifix [wits ;~(plug eol (plus tis))] sed))
    (stag 2 (ifix [wits ;~(plug eol (plus hep))] sed))
  ==
::
++  horz                                                ::  horizontal rule
  %+  stag  %horz
  %+  cold  ~  
  ;~  pose 
    ;~(plug (stun [0 3] ace) hep wits hep wits hep (star ;~(pose hep wite)))
    ;~(plug (stun [0 3] ace) tar wits tar wits tar (star ;~(pose tar wite)))
    ;~(plug (stun [0 3] ace) cab wits cab wits cab (star ;~(pose cab wite)))
  ==
::
++  html  (stag %html apex:xmlp)                        ::  html bloc
++  lasd                                                ::  top level list
  %+  stag  %list
  ;~  pose
    (stag & (lisd ;~(plug (star nud) dot)))
    (stag | (lisd hep))
    (stag | (lisd tar))
    (stag | (lisd lus))
  ==
::
++  lisd                                                ::  list funk
  |*  bus=_rule
  |=  tub=nail
  ^-  (like down)
  =+  chx=;~(plug (plus prn) (cold "\0a" eol))
  =-  ?~  q.pre
        pre
      :-  p.pre 
      (some (turn `wall`p.u.q.pre |=(a=tape [%lise (scan a apex)])) [p.pre ~])
  ^=  pre  %.  tub
  %+  most  ;~(pose ;~(plug wits eol) (easy ~))
  %+  cook  |=(a=wall `tape`(zing a))                   ::XX core dumps w/o cast
  ;~  plug
    %+  cook  zing
    ;~  pose
      (full ;~(pfix bus ace ;~(plug (plus prn) (easy ~))))
      ;~(pfix bus ace ;~(plug (plus prn) (cold "\0a" eol) (easy ~)))
    ==
    %-  star
    ;~  pose
      ;~(plug ;~(sfix eol ace ace) (cook welp chx))
      ;~(pfix ace ace (cook welp chx))
      (full ;~(pfix ace ace (plus prn)))
    ==
  ==
::
++  link                                                ::  link element
  %+  knee  *span  |.  ~+
  %+  stag  %link
  ;~  plug
    (ifix [sel ser] (plus ;~(pose emph stri cods (text ser))))
    ;~  pose
      %+  ifix  [pel per]
      ;~  plug  
        ;~(sfix (cook zing (most eol (plus ;~(less ace prn)))) ace)
        (cook some (ifix [doq doq] (plus ;~(less doq ;~(pose prn eol)))))
      ==
      %+  ifix  [pel per] 
      ;~(plug (cook zing (most eol (plus ;~(less per prn)))) (easy ~))
    ==
  ==
::
++  para  (stag %para (spas fail))                      ::  paragraph
++  quot                                                ::  blockquotes
  %+  stag  %quot 
  |=  tub=nail
  ^-  (like down)
  =-  ?~  q.pre
        [p.pre ~]
      (apex [[1 1] (welp p.u.q.pre q.q.u.q.pre)])
  ^=  pre  %.  tub  
  %+  cook  |=(a=wall `tape`(zing a))  
  %-  plus
  ;~  pfix  ;~(pose ;~(plug gar ace) gar)
    ;~(pose (cook welp ;~(plug (star prn) (cold "\0a" eol))) (full (star prn)))
  ==
::
++  sepa                                                ::  separate blocs
  %+  knee  *wall  |.  ~+
  =+  lin=;~(plug eol wits eol)
  %+  ifix  [(star whit) (star whit)]
  %+  more  ;~(plug eol wits (more wits eol))
    ;~  pose
      (sepl (cold "-" hep))
      (sepl (cold "*" tar))
      (sepl (cold "+" lus))
      (sepl (cook welp ;~(plug (star nud) (cold "." dot))))
      (plus ;~(less lin ;~(pose prn eol)))
    ==
::
++  sepl                                                ::  separate list
  |*  bus=_rule
  %+  cook  zing
  %+  most  ;~(pose ;~(plug wits eol) (easy ~))
  %+  cook  |=(a=wall `tape`(zing a))
  ;~  plug
    %+  cook  |=(a=wall `tape`(zing a))
    ;~  pose
      ;~(plug bus (cold " " ace) (plus prn) (cold "\0a" eol) (easy ~))
      (full ;~(plug bus (cold " " ace) (plus prn) (easy ~)))
    ==
  %-  star
    ;~  pose
      ;~  pfix  wits
        ;~(plug eol ace ace (cook welp ;~(plug (plus prn) (cold "\0a" eol))))
      ==
      ;~(plug ace ace (cook welp ;~(plug (plus prn) (cold "\0a" eol))))
      (full ;~(plug ace ace (plus prn)))
    ==
  ==
::
++  stri                                                ::  strikethrough text 
  %+  stag  %stri
  (ifix [(jest '~~') (jest '~~')] (plus ;~(pose emph cods link (text fail))))
::
++  text  |*(res=_rule (stag %text (plus ;~(less ;~(pose res dont) chrd))))
++  whit  (mask ~[`@`0x20 `@`0x9 `@`0xa])               ::  whitespace w/nl
++  wite  (mask ~[`@`0x20 `@`0x9])                      ::  whitespace
++  wits  (star wite)  
::
++  samp
"""
- 1asdfasf
  asdfafd
  asdfasf

  2asdasfd

  - 3asfasf
    asdfas

    4asdfasf

    - 5asfasf
      asdfas

      6asdfas

      - 7asfasf
        asdfas

        8asfass

      9asdfas

    10asdfasf

  11asfasfaf

- 13asdasdf
  adfadaf
"""
::
++  samp2 
"""
An h1 header
============

#An h1 header

Paragraphs are separated by a blank line.

2nd paragraph. *Italic*, **bold**, ***both*** and `monospace`.

Hard line//break

Links: [link](foo.com "title")

Strikethrough text: ~~foo~~

> Block quotes are
> written like so.
>
> They can span multiple paragraphs,
> if you like.

Itemized lists
look like:

* this one

* that one

* the other one

---

Indented code block:

    ++  sepa                                                ::  separate blocs
      %+  knee  *wall  |.  ~+
      =+  lin=;~(plug eol wits eol)
      %+  ifix  [(star whit) (star whit)]
      %+  more  ;~(plug eol wits (more wits eol))
        ;~  pose
          (sepl (cold "-" hep))
          (sepl (cold "*" tar))
          (sepl (cold "+" lus))
          (sepl (cook welp ;~(plug (star nud) (cold "." dot))))
          (plus ;~(less lin ;~(pose prn eol)))
        ==

Fenced code block

```
++  sepa                                                ::  separate blocs
  %+  knee  *wall  |.  ~+
  =+  lin=;~(plug eol wits eol)
  %+  ifix  [(star whit) (star whit)]
  %+  more  ;~(plug eol wits (more wits eol))
    ;~  pose
      (sepl (cold "-" hep))
      (sepl (cold "*" tar))
      (sepl (cold "+" lus))
      (sepl (cook welp ;~(plug (star nud) (cold "." dot))))
      (plus ;~(less lin ;~(pose prn eol)))
    ==
```
"""
      --
    ==
|=  *
|=  ~
^-  bowl 
:_  ~  :_  ~
:-  %$
!> 
`down`(scan samp2 apex)
