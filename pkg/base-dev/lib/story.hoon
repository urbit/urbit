/-  *story
|%
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