/-  *story
|%
++  tako-to-text
  |=  [=tako:clay]
  ^-  cord
  (crip "commit: {<`@uv`tako>}\0a")
++  proses-to-text
  |=  [proses=(set prose)]
  ^-  cord
  ::  XX code cleanup: factor out duplicate code
  =/  proses-list=(list prose)  ~(tap in proses)
  ?:  ?=(~ proses-list)  ''
  ?:  ?=([prose ~] proses-list)
    =/  [title=@t body=@t]  i.proses-list
    %-  crip
    ;:  welp
      "{(trip title)}"
      "\0a\0a"
      "{(trip body)}"
      "\0a"
    ==
  %-  crip
  %-  tail
  %^  spin  ;;((list prose) proses-list)  *tape  :: WHY DO WE NEED ;;
  |=  [[title=@t body=@t] state=tape]
  ^-  [prose tape]
  :-  [title body]
  ;:  welp
    state
    "|||\0a"
    "{(trip title)}"
    "\0a\0a"
    "{(trip body)}"
    "\0a"
  ==
--