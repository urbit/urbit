/-  neo
|%
+$  state-0  [%0 who=(set ship) title=@t]
+$  card     card:neo
+$  action
  $%  [%title title=@t]
      [%add =ship]
      [%del =ship]
  ==
--
^-  form:neo
|_  [=bowl:neo sta=* *]
+*  state  ;;(state-0 sta)
++  call
  |=  [old-state=* act=*]
  =+  ;;(=action act)
  ~&  call/act
  *(list card)
++  reduce
  |=  act=*
  ^-  *
  =+  ;;(=action act)
  =/  sta  state
  ?-  -.action
    %title  sta(title +.action)
    %add    sta(who (~(put in who.sta) ship.action))
    %del    sta(who (~(del in who.sta) ship.action))
  ==
++  init
  |=  old=(unit *)
  *state-0
++  take
  |=  =sign:neo
  *(list card:neo)
--
