/-  *story
!:
|%
::  XX generalize move to hoon.hoon
++  dif-ju
  |=  [a=story b=story]
  ^-  story
  ::  0 - b = -b. how do we negate a jug/"infinite" set lol?
  ::  a - 0 = a   (not impl.)
  ::  uno := (a-b) + (merged items in both a and b) + (b-a)
  ::  ret := (a-b) + (merged items in both a and b)  
  ::  ret = (~(int by a) uno)  :: preserve only the entries whose keys are in a
  ::
  ::
  ::  uno doesn't work if first argument is empty map
  ?:  =(~ a)  b
  ~&  a
  =/  uno=story
    ::~!  ((~(uno by story-a) story-a) |=(* *proses))
    ::~!  ((~(uno by `story`a) `story`a) |=(* *proses))
    %-  (~(uno by a) b)
    |=  [k=tako:clay proses-a=proses proses-b=proses]
    ^-  proses
    (~(dif in proses-a) proses-b)
  ::
  =/  ret=story  ::story-a  
    (~(int by a) uno)
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
++  tako-to-text
  |=  [=tako:clay]
  ^-  tape
  "commit: {<`@uv`tako>}\0a"
::
++  proses-to-text
  |=  [=proses]
  ^-  tape
  =/  proses-list=(list prose)  ~(tap in proses)
  ?:  ?=(~ proses-list)  ""
  ?:  ?=([prose ~] proses-list)
    (prose-to-text i.proses-list)
  %-  tail
  %^  spin  ;;((list prose) t.proses-list)  :: XX WHY DO WE NEED ;;
  (prose-to-text i.proses-list)
  |=  [prz=prose state=tape]
  ^-  [prose tape]
  :-  prz
  ;:  welp
    state
    "|||"
    "\0a"
    (prose-to-text prz)
  ==
::
++  prose-to-text
  |=  pro=prose
  =/  [title=@t body=@t]  pro
  ^-  tape
  ;:  welp
    "{(trip title)}"
    "\0a\0a"
    "{(trip body)}"
    "\0a"
  ==
::
::  Parsers
::
++  parse-commit
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
++  parse-body-single
  %-  star
  ;~  less
    ;~(pose (jest '|||\0a') (jest '---\0a'))
    (cook crip ;~(sfix (star prn) (just '\0a')))
  ==
::
++  parse-prose-single  ;~(plug parse-title parse-body-single)
++  parse-rest-proses     (star ;~(pfix (jest '|||\0a') parse-prose-single))
++  parse-proses        ;~(plug parse-prose-single parse-rest-proses)
++  parse-chapter       ;~(plug parse-commit parse-proses)
++  parse-story           (star ;~(sfix parse-chapter (jest '---\0a')))
::
::  Parse prose: a title, followed by a body
::
::  Parse proses: a prose, followed by zero or more proses, representing conflicts
::
::  Parse chapters (story entries): a commit followed by one or more proses
::
::  Parse story: zero or more chapters
::
::
--