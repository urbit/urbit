/-  *story
!:
^?
|%
::  XX generalize move to hoon.hoon
++  dif-ju
  |=  [a=story b=story]
  ^-  story
  ::  if 0 is the empty set,
  ::  a \ 0 = a
  ::  0 \ b = 0  :: anything in 0 but not in b is by definition 0
  ::
  ?:  =(~ a)  ~
  ::  uno := (a-b) + (merged items in both a and b) + (b-a)
  ::  ret := (a-b) + (merged items in both a and b)  
  ::  ret = (~(int by a) uno)  :: preserve only the entries whose keys are in a
  =/  uno=story
    %-  (~(uno by a) b)
    |=  [k=tako:clay proses-a=proses proses-b=proses]
    ^-  proses
    (~(dif in proses-a) proses-b)
  ::
  =/  ret=story  (~(int by a) uno)
  :: normalizing step, remove any keys with null sets, 
  :: which can occur if proses-a == proses-b above
  %-  ~(gas by *story)
  (skip ~(tap by ret) |=([k=* v=proses] ?=(~ v)))
::
++  uni-ju
  |=  [a=story b=story]
  ^-  story
  ::  0 + b = b
  ?:  =(~ a)  b
  %-  (~(uno by a) b)
  |=  [k=tako:clay proses-a=proses proses-b=proses]
  ^-  proses
  (~(uni in proses-a) proses-b)
::
::  Canonical textual representation
::
++  chapter-to-text
  |=  [=tako:clay =proses]
  ^-  wain
  :-  (crip "commit: {<`@uv`tako>}")
  %-  zing
  %+  join  `wain`~['|||']
  %+  turn  ~(tap in proses)
  |=  prose
  ^-  wain
  %-  to-wain:format
  (rap 3 title '\0a\0a' body ~)
::
::  Parsers
::
++  parse-commit-hash
  ;~  sfix
    ;~  pfix  (jest 'commit: ')
      (cook @uv ;~(pfix (jest '0v') viz:ag))
    ==
  ::
    (just '\0a')
  ==
::
++  parse-title
  ;~  sfix
    (cook crip (star prn))
    (jest '\0a\0a')
  ==
::
++  parse-body
  %+  cook  of-wain:format
  %-  star
  ;~  less
    ;~(pose (jest '|||\0a') (jest '---\0a'))
    (cook crip ;~(sfix (star prn) (just '\0a')))
  ==
::
++  parse-prose        ;~(plug parse-title parse-body)
++  parse-rest-proses  (star ;~(pfix (jest '|||\0a') parse-prose))
++  parse-proses       (cook silt ;~(plug parse-prose parse-rest-proses))
++  parse-chapter      ;~(plug parse-commit-hash parse-proses)
++  parse-story
  (cook ~(gas by *story) (star ;~(sfix parse-chapter (jest '---\0a'))))
::
::  N.B: If conflicting messages are written individually,
::  instead of under the same commit, we will overwrite previous entries
::  with later ones due to the nature of gas:by.
--
