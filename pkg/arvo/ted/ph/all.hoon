/-  spider
/+  *strandio, *ph-io
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
        %ph-boot-az
        %ph-boot-planet
        %ph-breach-hi
        %ph-breach-hi-cousin
        %ph-breach-multiple
        %ph-breach-sudden
        %ph-breach-sync
        %ph-change-file
        %ph-child-sync
        %ph-child-update
        %ph-hi
        %ph-hi-out-of-order
        %ph-hi-az
        %ph-hi-comet-az
        %ph-hi-cousin-az
        %ph-hi-linnup-az
        %ph-hi-linnup-az-backward
        %ph-hi-marbud-az
        %ph-hi-nephew-az
        %ph-hi-uncle-az
        %ph-moon-az
        %ph-peek
        %ph-second-cousin-hi
        %ph-tend
        %ph-flub
        %ph-big-flub
        %ph-ahoy
        %ph-gall-ames-desync
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
=/  cores=(list ?(%mesa %ames))  ~[%mesa %ames]
;<  global-now-1=@da  bind:m  get-time
|-  ^-  form:m
::  runn all test with the same network protocol core
::    (unless the test itself changes it)
::
:: ?~  cores
::   (pure:m !>(~))
:: ;<  ~  bind:m  (switch-network-core i.cores)
::
=*  loop  $
?^  threads
  ?:  =(%ph-all i.threads)
    loop(threads t.threads)
  ;<  now-1=@da       bind:m  get-time
  ;<  ~               bind:m  (flog-text "ph-all: {<i.threads>} started")
  ::  by default, turn probbing off for every tests
  ::
  ;<  ~  bind:m  (aqua-setup ahoy-on/|)
  ;<  =thread-result  bind:m  (await-thread i.threads *vase)
  ;<  ~               bind:m  (flog-text "ph-all: {<i.threads>} complete")
  ;<  now-2=@da       bind:m  get-time
  ~&  >>  "ph-all: {<i.threads>} took {<`@dr`(sub now-2 now-1)>}"
  loop(threads t.threads, results [[i.threads thread-result] results])
::
|-
=*  loop  $
?~  results
  ~&  "ph-all: all done"
  ;<  ~  bind:m  (flog-text "ph-all: all done")
  ;<  global-now-2=@da  bind:m  get-time
  ~&  >  ph-all-took/`@dr`(sub global-now-2 global-now-1)
  (pure:m !>(~))  ::TODO  maybe collate vases
?:  ?=(%& -.r.i.results)  loop(results t.results)
=*  name  n.i.results
=*  mess  p.r.i.results
;<  ~  bind:m  (flog-text "ph-all: {(trip name)} FAILED: {(trip -.mess)}")
;<  ~  bind:m  (flog-tang +.mess)
;<  ~  bind:m  (flog-text "")
loop(results t.results)
