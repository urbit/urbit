/-  *peerlist
::
|_  =peers
::
++  grab
  |%
  ++  noun  ^peers
  ::
  ++  txt
    |=  lines=wain
    %-  ~(gas by *^peers)
    %+  turn  lines
    |=  line=cord
    ^-  (pair ship relation)
    %+  rash  line
    |^  ;~((glue com) p (punt da) (punt da) f)
    ++  p   ;~(pfix sig fed:ag)
    ++  da  ;~(pfix sig (cook year when:so))
    ++  f   ;~(pfix dot ;~(pose (cold & (just 'y')) (cold | (just 'n'))))
    --
  ::
  ++  json
    =,  dejs:format
    |=  =json
    ^+  peers
    ?>  ?=([%o *] json)
    %-  ~(gas by *^peers)
    %+  turn  ~(tap by p.json)
    |=  [who=@t =^json]
    ^-  (pair ship relation)
    :-  (rash who ;~(pfix sig fed:ag))
    %.  json
    %-  ot
    :~  'we-since'^(mu di)
        'they-since'^(mu di)
        'public'^bo
    ==
  --
::
++  grow
  |%
  ++  noun  peers
  ::
  ++  txt
    ^-  wain
    %+  turn
      %+  sort  ~(tap by peers)
      |=  [[a=ship *] [b=ship *]]
      (lth a b)
    |=  [who=ship relation]
    ^-  cord
    %-  crip
    %-  zing
    %+  join  ","
    ^-  (list tape)
    :~  (scow %p who)
        ?^(we-since (scow %da u.we-since) "")
        ?^(they-since (scow %da u.they-since) "")
        (scow %f public)
    ==
  ::
  ++  json
    =,  enjs:format
    %-  pairs
    %+  turn  ~(tap by peers)
    |=  [=^ship relation]
    ^-  [@t ^json]
    :-  (scot %p ship)
    %-  pairs
    :~  'we-since'^?^(we-since (time u.we-since) ~)
        'they-since'^?^(they-since (time u.they-since) ~)
        'public'^b+public
    ==
  --
--