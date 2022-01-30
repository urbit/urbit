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
--