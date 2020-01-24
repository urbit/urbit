/-  spider
/+  *strandio
=,  strand=strand:spider
^-  thread:spider
|=  args=vase
=/  m  (strand ,vase)
=+  !<(group=(list @tas) args)
;<  =bowl:spider  bind:m  get-bowl
=/  threads=(list @tas)
  ?-    group
      ~
    :~  %ph-add
        %ph-hi
        %ph-child-sync
        %ph-breach-multiple
        %ph-breach-sudden
        %ph-breach-hi-cousin
        %ph-hi-linnup-az
        %ph-moon-az
    ==
  ::
      [%all ~]
    =+  .^(=arch %cy /(scot %p our.bowl)/home/(scot %da now.bowl)/ted/ph)
    %+  turn  (turn ~(tap by dir.arch) head)
    |=  =term
    (cat 3 'ph-' term)
  ::
      *
    (turn group |=(=term (cat 3 'ph-' term)))
  ==
::
=|  results=(list [@tas thread-result])
|-  ^-  form:m
=*  loop  $
?~  threads
  (pure:m !>(results))
;<  =thread-result  bind:m  (await-thread i.threads *vase)
;<  ~               bind:m  (flog-text "ph-all: {<i.threads>} complete")
loop(threads t.threads, results [[i.threads thread-result] results])
