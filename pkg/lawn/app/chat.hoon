/-  gall, chat-pub::, group-pub
::  agent-specific data structures
::
=>  |%
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
    ::  $pubs: publication state injected by gall
    ::
    +$  pubs
      %+  map  path
      [=rock:chat-pub meta=pub-meta:gall]
    ::  $step: agent activation result
    ::
    ::    fex: effects
    ::    state: new agent state
    ::    pubs: modified publications
    ::
    +$  step
      $:  fex=(list note)
          state=state-0
      ==
    --
:-  |%
    ++  sub  ~[%chat-pub %group-pub]
    ++  pub  ~[%chat-pub]
    +$  note  ::  outbound effect; required
      $%  note:gall
          [%poke ship poke-note]
          [%wave =path wave-gift]  ::  TODO: needs (unit rock)
      ==
    +$  poke-note
      $%  task
          task:groups
      ==
    ::  TODO: include agent path
    ::  +$  task
    ::    $%  [/chan/[id=@ta]/create ~]
    ::        [/chan/[id=@ta]/join ~]
    ::        [/chan/[id=@ta]/leave ~]
    ::        [/chan/[id=@ta]/add =memo:chat-pub]
    ::        [/chan/[id=@ta]/writ/[time=@da]/del ~]
    ::        [/chan/[id=@ta]/writ/[time=@da]/feel/add =term]
    ::        [/chan/[id=@ta]/writ/[time=@da]/feel/del =term]
    ::    ==
    ::
    +$  task  ::  inbound poke; required
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
    +$  wave-gift
      $%  [%chat-pub wave:chat-pub]
      ==
    --
::  gall uses +sub and +pub to inject a .pubs of the right type
::
|=  [=bowl state=state-0 =subs =pubs]
=/  nog  [fex=*(list note) =pubs]
=>  |%
    ++  wash-chat
      |=  [=path =wave:chat-pub]
      ^+  nog
      =/  =rock:chat-pub
        (wash:chat-pub rock:(~(got by pubs) path) wave)
      [[wave `rock] (~(put by pubs) path [wave rock])]
    ::
    ++  crag-chat
      |=  id=@ta
      ^+  nog
      =/  chan-path  (snoc wer.bowl (scot %ta id))
      :-  [[%crag %chat-pub chan-path `~] fex.nog]
      (~(put by pubs.nog) chan-path [*rock:chat-pub `~])
    ::
    ++  fend-chat
      |=  [id=@ta crew=(unit (set ship))]
      ^+  nog
      =/  chan-path  (snoc wer.bowl (scot %ta id))
      =/  pub  (~(got by pubs) chan-path)
      :-  [[%arvo %fend chan-path crew] fex.nog]
      (~(put by pubs.nog) chan-path pub(crew.meta crew))
    --
|%
++  this  .
++  abet  [(flop fex.nog) state]
++  on-init  this
++  on-load  |=(old=state-0 this(state old))
++  on-rift  |=(=ship this)
++  on-poke
  |=  =poke
  ^+  this
  ?-    -.poke
      [[%tas %chan] [%ta id=@ta] [%tas %create] ~]
    ?>  =(src.bowl our.bowl)
    this(nog (crag-chat id))
  ::
      [[%tas %chan] [%ta id=@ta] [%tas %join] ~]
    =-  this(nog (fend-chat id -))
    ?~  crew.meta.pub  ~
    `(~(put in u.crew.meta.pub) src.bowl)
  ::
      [[%tas %chan] [%ta id=@ta] [%tas %leave] ~]
    =-  this(nog (fend-chat id -))
    ?~  crew.meta.pub  ~
    `(~(del in u.crew.meta.pub) src.bowl)
  ::
      [[%tas %chan] [%ta id=@ta] [%tas %writ] [%tas %add] ~]
    =/  =memo:chat-pub  +.poke
    !!
  ::
      [[%tas %chan] [%ta id=@ta] [%tas %writ] [%tas %del] ~]
    !!
  ::
      [[%tas %chan] [%ta id=@ta] [%tas %feel] [%tas %add] ~]
    !!
  ::
      [[%tas %chan] [%ta id=@ta] [%tas %feel] [%tas %del] ~]
    !!
  ==
--
