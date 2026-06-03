/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
=/  cores=(list ?(%mesa %ames))  ~[%mesa %ames]
|-  ^-  form:m
=*  loop  $
?~  cores  (pure:m *vase)
::
=|  tids=drivers
;<  =_tids   bind:m  start-simple
;<  ~        bind:m  (init-ship ~bud fake=&)
;<  ~        bind:m  (dojo ~bud "|pass [%a %load {<i.cores>}]")
;<  ~        bind:m  (init-ship ~marbud fake=&)
;<  ~        bind:m  (dojo ~marbud "|pass [%a %load {<i.cores>}]")
;<  file=@t  bind:m  (touch-file ~bud %base %foo)
;<  ~        bind:m  (dojo ~bud "|merge %kids our %base")
;<  ~        bind:m  (check-file-touched ~marbud %base file)
;<  ~        bind:m  (end tids)
$(cores t.cores)
