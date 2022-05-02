/-  gall, chat-pub::, group-pub
::  agent-specific data structures
::
=>  |%
    +$  note
      $%  note:gall
          [%poke ship poke]
          [%wave wave]
      ==
    ::
::  +$  poke
::    $%  [/chan/[id=@ta]/create ~]
::        [/chan/[id=@ta]/join ~]
::        [/chan/[id=@ta]/leave ~]
::        [/chan/[id=@ta]/add =memo:chat-pub]
::        [/chan/[id=@ta]/writ/[time=@da]/del ~]
::        [/chan/[id=@ta]/writ/[time=@da]/feel/add =term]
::        [/chan/[id=@ta]/writ/[time=@da]/feel/del =term]
::    ==
    ::
    +$  poke
      $%  $:  [[%tas %chan] [%ta id=@ta] [%tas %create] ~]
              ~
          ==
          $:  [[%tas %chan] [%ta id=@ta] [%tas %join] ~]
              ~
          ==
          $:  [[%tas %chan] [%ta id=@ta] [%tas %leave] ~]
              ~
          ==
          $:  [[%tas %chan] [%ta id=@ta] [%tas %writ] [%tas %add] ~]
              =memo:chat-pub
          ==
          $:  [[%tas %chan] [%ta id=@ta] [%tas %writ] [%tas %del] ~]
              =time
          ==
          $:  [[%tas %chan] [%ta id=@ta] [%tas %feel] [%tas %add] ~]
              [=time =term]
          ==
          $:  [[%tas %chan] [%ta id=@ta] [%tas %feel] [%tas %del] ~]
              [=time =term]
      ==  ==
    ::
::  +$  wave
::    $%  [/chan/[id=@ta] =wave:chat-pub]
::    ==
    +$  wave
      $%  $:  [[%tas %chan] [%ta id=@ta] ~]
              =wave:chat-pub]
      ==  ==
    ::
    +$  state-0  [%0 ~]
    +$  subs
      $:  $=  chat-pub
          %+  map  [=ship =path]
          [=rock:chat-pub meta=sub-meta:gall]
      ::
          $=  group-pub
          %+  map  [=ship =path]
          [=rock:group-pub meta=sub-meta:gall]
      ==
    ::  $pubs-in: publication state injected by gall
    ::
    +$  pubs-in
      %+  map  path
      [=rock:chat-pub meta=pub-meta:gall]
    ::  $pubs-out: publication state after agent mutations
    ::
    +$  pubs-out
      %+  map  path
      (list [=wave =rock]:chat-pub)
    ::  $step: agent activation result
    ::
    ::    fex: effects
    ::    state: new agent state
    ::    pubs: modified publications
    ::
    +$  step
      $:  fex=(list note)
          state=state-0
          pubs=pubs-out
      ==
    --
::  sub: marks we subscribe to; pub: marks we publish
::
:-  [sub=~[%chat-pub %group-pub] pub=~[%chat-pub]]
::  gall uses .sub and .pub to inject a .pubs of the right type
::
=|  fex=(list note:agent:gall)
=|  =pubs-out
|_  [=bowl state=state-0 =subs =pubs-in]
++  on-poke
  =>  |%
      ++  get-chat-rock
        |=  =path
        ^-  rock
        =/  out  (~(get by pubs-out) path)
        ?^  out
          rock:(rear u.out)
        rock:(~(got by pubs-in) path))
      ::
      ++  wash-chat
        |=  [=path =wave:chat-pub]
        ^+  pubs-out
        %+  ~(put by pubs-out)  path
        %+  snoc  (~(gut by pubs-out) path ~)
        [wave (wash:chat-pub (get-chat-rock path) wave)]
      --
  |%
  ++  init  `state
  ++  load  |=(old=state-0 `old)
  ++  rift  |=(=ship `state)
  ++  chan
    :-  %ta
    |_  id=@ta
    +*  chan-path  (snoc wer.bowl (scot %ta id))
    ::
    ++  create
      :-  ,~
      |=  [rest=path arg=~]
      ?>  =(src.bowl our.bowl)
      =/  crew=(unit (set ship))  (some ~)
      [[%arvo %crag %chat-pub chan-path crew]~ state]
    ::
    ++  join
      :-  ,~
      |=  [rest=path arg=~]
      =/  pub  (~(got by pubs) ship chan-path)
      =/  crew=(unit (set ship))
        ?~  crew.meta.pub  ~
        `(~(put in u.crew.meta.pub) src.bowl)
      [[%arvo %fend chan-path crew]~ state]
    ::
    ++  leave
      :-  ,~
      |=  [rest=path arg=~]
      =/  pub  (~(got by pubs) ship chan-path)
      =/  crew
        ?~  crew.meta.pub  ~
        `(~(del in u.crew.meta.pub) src.bowl)
      [[%arvo %fend chan-path crew]~ state]
    ::
    ++  writ
      |%
      ++  add
        :-  memo:chat-pub
        |=  [rest=path =memo:chat-pub]

        :_  state
        [%give %wave]~
      ++  del
        :-  @da
        |=  [rest=path @da]
      --
    --
  --
--
