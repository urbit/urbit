/-  spider, aquarium
/+  *ph-io
/*  pub-agent  %hoon  /tests/app/pub/hoon
/*  sub-agent  %hoon  /tests/app/sub/hoon
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
=/  cores=(list ?(%mesa %ames))  ~[%mesa %ames]
|-  ^-  form:m
?~  cores  (pure:m *vase)
;<  ~  bind:m  start-simple
::
=*  loop  $
;<  ~  bind:m  (init-ship ~bud fake=&)
;<  ~  bind:m  (dojo ~bud "|pass [%a %load {<i.cores>}]")
;<  ~  bind:m  (init-ship ~dev fake=&)
;<  ~  bind:m  (dojo ~dev "|pass [%a %load {<i.cores>}]")
::
;<  ~  bind:m  (send-hi ~bud ~dev)
::
;<  ~  bind:m  (dojo ~bud "|mount %base")
;<  ~  bind:m  (dojo ~dev "|mount %base")
::
;<  ~  bind:m  (copy-file ~bud /app/sub/hoon sub-agent)
;<  ~  bind:m  (dojo ~bud "|start %sub")
::  poke a non-running agent
::
;<  ~  bind:m  (dojo ~bud ":sub [%sub ~dev %pub]")
::  %prod, so we resend the /gf plea that whas dropped on first contact
::
;<  ~  bind:m  (dojo ~bud "|pass [%a %prod [~dev]~]")
::  check that ~dev has halted this flow
::
;<  ~  bind:m  (wait-for-has-halt ~dev ~bud %pub)  :: XX
::  check that remote flubs are received
::
;<  ~  bind:m  (wait-for-flub ~bud ~dev %pub)
::  XX check that proding doesn't actually send the flubbed poke again
::
;<  ~  bind:m  (dojo ~bud "|pass [%a %prod [~dev]~]")
::
;<  ~  bind:m  (copy-file ~dev /app/pub/hoon pub-agent)
;<  ~  bind:m  (dojo ~dev "|start %pub")
::   check that the flow is not halted anymore
::
;<  ~  bind:m  (wait-for-del-halt ~dev ~bud %pub)
::   check that the %spur is sent
::
;<  ~  bind:m  (wait-for-spur ~bud ~dev %pub)
:: XX we should scry into ~bud for no entries in the .pit
::
;<  ~  bind:m  (sleep ~s1)
::  subscribe of ~dev for %pub gifts
::
=/  =aqua-event:aquarium
  :+  %event  ~dev
  [/g/aqua/watch/pub %deal [~dev ~dev /] %pub %watch /subs]
;<  ~      bind:m  (send-events aqua-event ~)
::  now we are going to give an initial fact and wait for it
::
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/hola 45]~")
=/  gate
    |*  [typ=mold exp=noun]
    |=  [=mark val=noun]
    ~|  [exp val]
    ?+  mark  %.n
      %pub-fact  =(exp ;;(typ val))
    ==
;<  =noun  bind:m
  (wait-for-fact ~dev %pub-fact /aqua/watch/pub (gate ,(list [path @]) [/hola 45]~))
::
;<  ~  bind:m  end
$(cores t.cores)
