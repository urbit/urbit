/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
|^
=/  m  (strand ,vase)
;<  ~                bind:m  start-simple
;<  ~                bind:m  (raw-ship ~bud ~)
;<  ~                bind:m  (raw-ship ~marbud ~)
;<  [path @t]        bind:m  (modify ~bud %home)
;<  [=path file=@t]  bind:m  (modify ~bud %kids)
;<  ~                bind:m  (check-touched ~marbud %kids path file)
;<  ~                bind:m  end-simple
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
    %^  cat  3  (get-val /mar/js/hoon)
    '  ~&  >  new-val=new-val  .'
  =/  js-contents
    %^  cat  3  (get-val /app/landscape/js/channel/js)
    'extra'
  =/  files
    :~  [/sys/zuse/hoon zuse-contents]
        [/mar/js/hoon mar-contents]
        [/app/landscape/js/channel/js js-contents]
    ==
  ;<  ~  bind:m  (send-events (insert-files:util her desk files))
  (pure:m /app/landscape/js/channel/js js-contents)
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
