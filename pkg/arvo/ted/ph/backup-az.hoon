/-  spider
/+  *ph-io, io=strandio, store=chat-store, util=ph-util
=,  strand=strand:spider
^-  thread:spider
=>  |%
    ++  print-chat-store-state
      |=  =ship
       =/  m  (strand ,~)
       ;<  =bowl:spider  bind:m  get-bowl
       =/  pax
         /i/(scot %p ship)/gx/(scot %p ship)/chat-store/(scot %da now.bowl)/all/noun/noun
       ~&  ;;(inbox:store +:(scry-aqua:util noun our.bowl now.bowl pax))
       (pure:m ~)
    ::
    ++  boot-chat
      |=  =ship
      =/  m  (strand ,~)
      ^-  form:m
      ;<  ~  bind:m  (boot-app ship %chat-store)
      ;<  ~  bind:m  (boot-app ship %metadata-store)
      ;<  ~  bind:m  (boot-app ship %permission-store)
      ;<  ~  bind:m  (boot-app ship %chat-hook)
      ;<  ~  bind:m  (boot-app ship %chat-view)
      ;<  ~  bind:m  (boot-app ship %group-store)
      ;<  ~  bind:m  (boot-app ship %group-hook)
      ;<  ~  bind:m  (boot-app ship %permission-group-hook)
      ;<  ~  bind:m  (boot-app ship %permission-hook)
      ;<  ~  bind:m  (boot-app ship %metadata-hook)
      (pure:m ~)
    --
|=  vase
=/  m  (strand ,vase)
=/  path=tape
  "/~/~dev/test-chat"
;<  az=tid:spider  bind:m
  start-azimuth
;<  ~  bind:m  (spawn az ~bud)
;<  ~  bind:m  (spawn az ~dev)
;<  ~  bind:m  (real-ship az ~bud)
;<  ~  bind:m  (wait-for-goad ~bud)
;<  ~  bind:m  (boot-chat ~bud)
;<  ~  bind:m  (real-ship az ~dev)
;<  ~  bind:m  (wait-for-goad ~dev)
;<  ~  bind:m  (boot-chat ~dev)
;<  ~  bind:m  (send-hi ~bud ~dev)
;<  ~  bind:m
  %+  dojo  ~dev
  ":chat-view &chat-view-action [%create 'test' '' {path} {path} %channel ~ %.y]"
;<  =bowl:spider  bind:m  get-bowl
;<  ~  bind:m  (sleep:io ~s5)
;<  ~  bind:m
  %+  dojo  ~dev
  ":chat-hook &chat-action [%message {path} 0v10 1 ~dev {<now.bowl>} %text 'test']"
;<  ~  bind:m  (sleep:io ~s5)
;<  ~  bind:m
  %+  dojo  ~bud
  ":chat-view &chat-view-action [%join ~dev {path} %.y]"
;<  ~  bind:m  (sleep:io ~s5)
;<  ~  bind:m
  %+  dojo  ~bud
  ":chat-hook &chat-action [%message {path} 0v11 2 ~bud {<now.bowl>} %text 'response']"
;<  ~  bind:m  (sleep:io ~s5)
;<  backup=unix-event  bind:m
  (backup-ship ~dev)
;<  ~  bind:m  (breach-and-hear az ~dev ~bud)
;<  ~  bind:m  (restore-ship az ~dev backup)
;<  ~  bind:m  (wait-for-goad ~dev)
;<  ~  bind:m  (boot-chat ~dev)
;<  ~  bind:m  (send-hi ~dev ~bud)
;<  ~  bind:m  (send-hi ~bud ~dev)
;<  ~  bind:m
  %+  dojo  ~bud
  ":chat-hook &chat-action [%message {path} 0v12 3 ~bud {<now.bowl>} %text 'after breach']"
;<  ~  bind:m  (sleep:io ~s5)
;<  ~  bind:m
  %+  dojo  ~bud
  ":chat-store +dbug"
;<  ~  bind:m
  %+  dojo  ~dev
  ":chat-store +dbug"
;<  ~  bind:m  (print-chat-store-state ~dev)
;<  ~  bind:m  (print-chat-store-state ~bud)
(pure:m *vase)
