/-  gall
/%  chat   %chat
/%  group  %group
::
|%
::  $state-0: versioned agent state
::
+$  state-0
  $:  %0
      buds=(map @ta (set ship))
  ==
::  $sub-state: subscription state injected by gall
::
+$  sub-state
  $:  $=  chat
      %+  map  [=ship =path]
      [=rock:chat meta=sub-meta:gall]
  ::
      $=  group
      %+  map  [=ship =path]
      [=rock:group meta=sub-meta:gall]
  ==
::  $pub-state: publication state injected by gall
::
+$  pub-state
  %+  map  path
  [=rock:chat meta=pub-meta:gall]
::  $step: agent activation result
::
::    fex: effects
::    state: new agent state
::
+$  step
  $:  fex=(list move)
      state=state-0
  ==
+$  poke-note
  $%  task
      task:groups
  ==
::  $wave-note: send a wave to a publication
::
::    If the agent applied the diff itself, .rock contains the
::    result; otherwise, .rock is ~ and Gall will apply the diff itself.
::    Remote subscribers should receive a mug of the new state with each
::    diff as a sanity check to make sure the publishing agent applied
::    the diff correctly.
::
+$  wave-note
  $%  $:  [[%tas %chan] [%ta id=@ta] ~]
          [=wave:chat rock=(unit rock:chat)]
  ==  ==
::  $wave-sign: receive an update on a subscription
::
+$  wave-sign
  $%  $:  [[%tas %chan] [%ta id=@ta] ~]
          [=wave =rock]:chat
  ==  ==
::  $scry-response: path-tagged result of +on-peek
::
+$  scry-response
  $%  [[[%tas %q] [%p @p] [%tas %chan] [%ta @ta] ~] ?]
      [[[%tas %x] [%p @p] [%tas %chan] [%ta @ta] ~] rock:chat]
  ==
::
++  subs  ~[%chat %group]  ::  marks we subscribe to
++  pubs  ~[%chat]         ::  marks we publish
::  $move: outbound effect
::
+$  move
  $%  [%pass =wire =note]
      ::  [%give =gift]  ::  are there any gifts?
  ==
::  $note: outbound request
::
+$  note
  $%  note:agent:gall
      [%poke =ship poke-note]
      [%wave =path wave-note]
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
::  $task: inbound request
::
+$  task
  $:  [%tas %lawn]
      [%tas %chat]
      [%tas %chan]
      [%ta id=@ta]
      $%  [[[%tas %create] ~] ~]
          [[[%tas %join] ~] ~]
          [[[%tas %leave] ~] ~]
          [[[%tas %writ] [%tas %add] ~] sent=@da content=@t]
          [[[%tas %writ] [%tas %del] ~] =time]
          [[[%tas %feel] [%tas %add] ~] =time =term]
          [[[%tas %feel] [%tas %del] ~] =time =term]
  ==  ==
::  $sign: inbound response
::
+$  sign
  $%  sign:agent:gall
      wave-sign
  ==
--
::  gall uses +subs and +pubs to inject a $sub-state and $pub-state
::
|=  [=bowl state=state-0 subs=sub-state pubs=pub-state]
=/  nog  [fex=*(list note) =pubs]
=>  |%
    ++  emit  |=(m=move nog(fex [m fex.nog]))
    ++  chan-path  |=(id=@ta (snoc wer.bowl (scot %ta id)))
    ++  wave-chat
      |=  [id=@ta =wave:chat]
      ^+  nog
      =/  pax  (chan-path id)
      =/  =rock:chat
        (wash:chat rock:(~(got by pubs.nog) pax) wave)
      [[wave `rock] (~(put by pubs.nog) pax [wave rock])]
    ::
    ++  crag-chat
      |=  id=@ta
      ^+  nog
      =/  pax  (chan-path id)
      :-  [[%crag %chat pax] fex.nog]
      (~(put by pubs.nog) pax [*rock:chat])
    ::
    ++  is-member
      |=  [=ship id=@ta]
      ^-  ?
      (~(has ju buds.state) id ship)
    --
|%
++  this  .
++  abet  [(flop fex.nog) state]
++  on-init  this
++  on-load  |=(old=state-0 this(state old))
++  on-rift  |=(=ship this)
++  on-peek  ::  TODO types?
  |=  =path
  ^-  (unit scry-response)
  ?+    path  ~
      [[%tas %q] [%p ship=@p] [%tas %chan] [%ta id=@ta] ~]
    `[path (is-member ship id)]
  ==
::
++  on-poke
  |=  =task
  ^+  this
  =*  her  ship.src.bowl
  =*  id=@ta  id.task
  ?-    |4.task
      [[%tas %create] ~]
    ?>  =(her our.bowl)
    this(nog (crag-chat id))
  ::
      [[%tas %join] ~]
    this(buds.state (~(put ju buds.state) id her))
  ::
      [[%tas %leave] ~]
    =.  nog  (emit %fend +:(chan-path id))  ::  rekey
    this(buds.state (~(del ju buds.state) id her))
  ::
      [[%tas %writ] [%tas %add] ~]
    ?>  (is-member her id)
    this(nog (wave-chat id [%chan %add [now ~] her +.poke]))
  ::
      [[%tas %writ] [%tas %del] ~]
    ?>  (is-member her id)
    this(nog (wave-chat id [%chan %del her +.poke]))
  ::
      [[%tas %feel] [%tas %add] ~]
    ?>  (is-member her id)
    this(nog (wave-chat id [%chan %add-feel her +.poke]))
  ::
      [[%tas %feel] [%tas %del] ~]
    ?>  (is-member her id)
    this(nog (wave-chat id [%chan %del-feel her +.poke]))
  ==
::
++  on-sign
  |=  [=wire =sign]
  ^+  this
  ?+    -.sign  this
      ?(%poke-ack %gaze-ack)
    ~?  ?=(^ err.sign)  lawn-error+sign
    this
  ==
--
