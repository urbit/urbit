/-  spider
/+  ph-io
=,  thread=thread:spider
^-  imp:spider
|=  =bowl:mall
=/  m  (thread ,~)
~&  >  'STARTING'
;<  ~  bind:m  start-simple:ph-io
~&  >  'START ~BUD'
;<  ~  bind:m  (raw-ship:ph-io ~bud ~)
~&  >  'DOJO'
;<  ~  bind:m  (dojo:ph-io ~bud "[%test-result (add 2 3)]")
~&  >  'WAIT'
;<  ~  bind:m  (wait-for-output:ph-io ~bud "[%test-result 5]")
~&  >  'STOPPING'
;<  ~  bind:m  end-simple:ph-io
~&  >  'DONE'
(pure:m ~)
