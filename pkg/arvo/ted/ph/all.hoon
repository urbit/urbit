/-  spider
/+  *strandio
=,  strand=strand:spider
^-  thread:spider
|=  args=vase
=/  m  (strand ,vase)
=+  !<([~ group=(list @tas)] args)
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
    =+  .^(=arch %cy /(scot %p our.bowl)/base/(scot %da now.bowl)/ted/ph)
    %+  turn  (sort (turn ~(tap by dir.arch) head) aor)
    |=  =term
    (cat 3 'ph-' term)
  ::
      *
    (turn group |=(=term (cat 3 'ph-' term)))
  ==
::
=|  results=(list [n=@tas r=thread-result])
|-  ^-  form:m
=*  loop  $
?^  threads
  ?:  =(%ph-all i.threads)
    loop(threads t.threads)
  ;<  ~               bind:m  (flog-text "ph-all: {<i.threads>} started")
  ;<  =thread-result  bind:m  (await-thread i.threads *vase)
  ;<  ~               bind:m  (flog-text "ph-all: {<i.threads>} complete")
  loop(threads t.threads, results [[i.threads thread-result] results])
::
|-
=*  loop  $
?~  results  (pure:m !>(~))  ::TODO  maybe collate vases
?:  ?=(%& -.r.i.results)  loop(results t.results)
=*  name  n.i.results
=*  mess  p.r.i.results
;<  ~  bind:m  (flog-text "ph-all: {(trip name)} failed: {(trip -.mess)}")
;<  ~  bind:m  (flog-tang +.mess)
;<  ~  bind:m  (flog-text "")
loop(results t.results)
