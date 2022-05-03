/-  gall, chat-pub::, group-pub
::  agent-specific data structures; unofficial optional helper core
::
|%
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
+$  poke-note
  $%  task
      task:groups
  ==
+$  wave-gift
  $%  [%chat-pub =wave:chat-pub rock=(unit rock:wave:chat-pub)]
  ==
+$  wave-sign
  $%  [%chat-pub [=wave =rock]:chat-pub]
  ==
--
::  agent descriptor core, all arms required
|%
++  sub-marks  ~[%chat-pub %group-pub]
++  pub-marks  ~[%chat-pub]
+$  note  ::  outbound effect
  $%  $<(%poke $<(%wave note:gall))
      [%poke =ship poke-note]
      [%wave =path wave-gift]
  ==
::  +$  task
::    $%  [/lawn/chat/chan/[id=@ta]/create ~]
::        [/lawn/chat/chan/[id=@ta]/join ~]
::        [/lawn/chat/chan/[id=@ta]/leave ~]
::        [/lawn/chat/chan/[id=@ta]/add sent=@da content=@t]
::        [/lawn/chat/chan/[id=@ta]/del =time]
::        [/lawn/chat/chan/[id=@ta]/writ/feel/add =time =term]
::        [/lawn/chat/chan/[id=@ta]/writ/feel/del =time =term]
::    ==
::
+$  task  ::  inbound poke
  $:  [%tas %lawn]
      [%tas %chat]
      [%tas %chan]
      [%ta id=@ta]
      $%  [[%tas %create] ~]
          [[%tas %join] ~]
          [[%tas %leave] ~]
          $:  [[%tas %writ] [%tas %add] ~]
              [sent=@da content=@t]
          ==
          $:  [[%tas %writ] [%tas %del] ~]
              =time
          ==
          $:  [[%tas %feel] [%tas %add] ~]
              [=time =term]
          ==
          $:  [[%tas %feel] [%tas %del] ~]
              [=time =term]
  ==  ==  ==
+$  sign
  $%  $<(%wave sign:gall)
      wave-sign
  ==
--
::  gall uses +sub-marks and +pub-marks to inject a .pubs of the right type
::
|=  [=bowl state=state-0 =subs =pubs]
=/  nog  [fex=*(list note) =pubs]
=>  |%
    ++  chan-path  |=(id=@ta (snoc wer.bowl (scot %ta id)))
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
      =/  pax  (chan-path id)
      :-  [[%crag %chat-pub pax `~] fex.nog]
      (~(put by pubs.nog) pax [*rock:chat-pub `~])
    ::
    ++  fend-chat
      |=  [id=@ta add=? =ship]
      ^+  nog
      =/  pax  (chan-path id)
      =/  pub  (~(got by pubs) pax)
      =/  crew
        ?~  crew.meta.pub  ~
        ?:  add
          `(~(put in u.crew.meta.pub) src.bowl)
        `(~(del in u.crew.meta.pub) src.bowl)
      :-  [[%arvo %fend pax crew] fex.nog]
      (~(put by pubs.nog) pax pub(crew.meta crew))
    ::
    ++  is-member
      |=  [=ship id=@ta]
      ^-  ?
      =/  crew  crew:(~(got by pubs) (chan-path id))
      ?~  crew
        &
      (~(has in u.crew) ship)
    --
|%
++  this  .
++  abet  [(flop fex.nog) state]
++  on-init  this
++  on-load  |=(old=state-0 this(state old))
++  on-rift  |=(=ship this)
++  on-poke
  |=  =task
  ^+  this
  =*  id=@ta  id.task
  ?-    |4.task
      [[%tas %create] ~]
    ?>  =(src.bowl our.bowl)
    this(nog (crag-chat id))
  ::
      [[%tas %join] ~]
    this(nog (fend-chat id & src.bowl))
  ::
      [[%tas %leave] ~]
    this(nog (fend-chat id | src.bowl))
  ::
      [[%tas %writ] [%tas %add] ~]
    ?>  (is-member src.bowl id)
    this(nog (wave-chat id [%chan %add [now ~] src.bowl +.poke]))
  ::
      [[%tas %writ] [%tas %del] ~]
    ?>  (is-member src.bowl id)
    this(nog (wave-chat id [%chan %del src.bowl +.poke]))
  ::
      [[%tas %feel] [%tas %add] ~]
    ?>  (is-member src.bowl id)
    this(nog (wave-chat id [%chan %add-feel src.bowl +.poke]))
  ::
      [[%tas %feel] [%tas %del] ~]
    ?>  (is-member src.bowl id)
    this(nog (wave-chat id [%chan %del-feel src.bowl +.poke]))
  ==
::
++  on-sign
  |=  =sign:gall
  ^+  this
  ?+    -.sign  this
      ?(%poke-ack %gaze-ack)
    ~?  ?=(^ err.sign)  lawn-error+sign
    this
  ==
--
