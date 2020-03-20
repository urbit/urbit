/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=+  !<([scratch=desk real=desk ~] arg)
|^
=/  m  (strand ,vase)
^-  form:m
;<  apps=(map path vase)  bind:m  load-apps
(pure:m !>((~(run by apps) mug)))
::
++  scratch-path
  |=  [=bowl:spider =path]
  (weld /(scot %p our.bowl)/[scratch]/(scot %da now.bowl) path)
::
++  load-apps
  =/  m  (strand ,(map path vase))
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl:strandio
  =+  .^(=arch %cy (scratch-path bowl /app))
  =/  apps  ~(tap in ~(key by dir.arch))
  =/  rails
    %-  malt
    %+  murn  apps
    |=  =term
    ^-  (unit [^term rail:ford])
    =+  .^(=app=^arch %cy (scratch-path bowl /app/[term]))
    ?.  (~(has by dir.app-arch) %hoon)
      ~
    `[/[term] our.bowl^scratch /hoon/[term]/app]
  (build-cores:strandio rails)
--
