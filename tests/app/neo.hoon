/-  neo
/+  *test-agent
/=  neo-agent  /app/neo
/=  chat-shrub   /lib/chat
|%
++  scry-handler
  |=  =(pole knot)
  ^-  (unit vase)
  =^  view=@t  pole
    ?>  ?=([@ *] pole)
    [-.pole +.pole]
  =.  pole  (slag 3 pole)
  ~&  pole
  `!>(!>(chat-shrub))
+$  card  card:agent:gall
++  make-chat
  |=  [our=ship =pith]
  ^-  note:neo
  :-  `^pith`[p/our pith]
  [%make %chat ~ ~]
++  test-neo
  %-  eval-mare
  =/  m  (mare ,~)
  ^-  form:m
  ;<  caz=(list card)  bind:m
    (do-init %neo neo-agent)
  ;<  ~  bind:m
    (ex-cards caz ~)
  ;<  ~  bind:m
    (set-scry-gate scry-handler)
  ;<  =bowl  bind:m  get-bowl
  ;<  caz=(list card)  bind:m
    (do-poke %noun !>((make-chat our.bowl #/foo)))
  (pure:m ~)
--
