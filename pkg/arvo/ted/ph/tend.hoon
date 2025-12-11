/-  spider
/+  *ph-io, strandio
/*  tend-agent  %hoon  /tests/app/tend/hoon
=,  strand=strand:spider
=<  all
|% 
++  tend
  |=  zuse=@ud
  =/  m  (strand ,~)
  ;<  ~  bind:m  (dojo ~bud ":tend [%tend /foo /baz %kelvin %zuse {(scow %ud zuse)}]")
  ;<  ~  bind:m  (sleep:strandio ~s2)
  ;<  ~  bind:m  (dojo ~bud ":tend +dbug %bowl")
  (pure:m ~)
::
++  keen-wait-for-result
  |=  [cas=@ud zuse=@ud]
  =/  m  (strand ,~)
  ;<  ~  bind:m  (dojo ~dev ":tend [%keen ~bud {(scow %ud cas)} /foo/baz]")
  ;<  ~  bind:m  (wait-for-output ~dev "kal=[lal=%zuse num={(scow %ud zuse)}]")
  (pure:m ~)
::
++  setup
  =/  m  (strand ,~)
  ;<  ~  bind:m  start-simple
  ::  testing usual case
  ;<  ~  bind:m  (init-ship ~bud &)
  ;<  ~  bind:m  (init-ship ~dev &)
  ;<  ~  bind:m  (dojo ~bud "|mount %base")
  ;<  ~  bind:m  (dojo ~dev "|mount %base")
  ;<  ~  bind:m  (copy-file ~bud /app/tend/hoon tend-agent)
  ;<  ~  bind:m  (copy-file ~dev /app/tend/hoon tend-agent)
  ;<  ~  bind:m  (dojo ~bud "|start %tend")
  ;<  ~  bind:m  (dojo ~dev "|start %tend")
  (pure:m ~)
::
++  all
  ^-  thread:spider
  |=  vase
  =/  m  (strand ,vase)
  ;<  ~  bind:m  test-normal
  ;<  ~  bind:m  test-larval-ames
  (pure:m *vase)
::
++  test-larval-ames
  =/  m  (strand ,~)
  ;<  ~  bind:m  setup
  ;<  ~  bind:m  (dojo ~bud ":tend [%germ /foo]")
  ;<  ~  bind:m  (sleep:strandio ~s2)
  ;<  ~  bind:m  (tend zuse)
  ;<  ~  bind:m  (keen-wait-for-result 1 zuse)
  =/  zuse  (dec zuse)
  ;<  ~  bind:m  (tend zuse)
  ;<  ~  bind:m  (keen-wait-for-result 2 zuse)
  ;<  ~  bind:m  end
  (pure:m ~)
::
++  test-normal
  =/  m  (strand ,~)
  ;<  ~  bind:m  setup
  ;<  ~  bind:m  (send-hi ~bud ~dev) :: make sure both ames have metamorphosed
  ;<  ~  bind:m  (dojo ~bud ":tend [%germ /foo]")
  ;<  ~  bind:m  (sleep:strandio ~s2)
  ;<  ~  bind:m  (tend zuse)
  ;<  ~  bind:m  (keen-wait-for-result 1 zuse)
  =/  zuse  (dec zuse)
  ;<  ~  bind:m  (tend zuse)
  ;<  ~  bind:m  (keen-wait-for-result 2 zuse)
  ;<  ~  bind:m  end
  (pure:m ~)
--
