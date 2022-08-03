/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
|^
=/  m  (strand ,vase)
;<  ~                bind:m  start-simple
;<  ~                bind:m  (init-ship ~bud &)
;<  ~                bind:m  (init-ship ~marbud &)
;<  *                bind:m  (modify ~bud %base)
;<  [=path file=@t]  bind:m  (modify ~bud %kids)
;<  ~                bind:m  (check-touched ~marbud %kids path file)
;<  ~                bind:m  end
(pure:m *vase)
::
++  modify
  |=  [her=@p =desk]
  =/  m  (strand ,[path @t])
  ^-  form:m
  ;<  ~        bind:m  (mount her desk)
  ;<  our=@p   bind:m  get-our
  ;<  now=@da  bind:m  get-time
  |^
  =/  zuse-contents
    %^  cat  3  '=/  new-val  57  '
    (get-val /sys/zuse/hoon)
  =/  mar-contents
    %^  cat  3  (get-val /mar/hoon/hoon)
    ::TODO  doesn't get picked up somehow
    :: '  ~&  >  new-val=new-val  .'
    '  ~&  >  %testing  .'
  =/  files
    :~  ::[/sys/zuse/hoon zuse-contents]
        [/mar/hoon/hoon mar-contents]
    ==
  ;<  ~  bind:m  (send-events (insert-files:util her desk files))
  (pure:m /mar/hoon/hoon mar-contents)
  ::
  ++  aqua-path
    |=  =path
    ;:  weld
        /i/(scot %p her)/cx/(scot %p her)/[desk]/(scot %da now)
        path
        /noun
    ==
  ::
  ++  get-val
    |=  =path
    (need (scry-aqua:util (unit @) our now (aqua-path path)))
  --
--
