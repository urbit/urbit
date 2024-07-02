::  -new-app: creates a desk from example agent repo
::  Derived from pkova/deployer/ted/sync.hoon
::
/-  spider
/+  strandio
=>
|%
++  test  !!
--
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
;<  =bowl:spider  bind:m  get-bowl:strandio
=/  desk  ;;(term +<.q.arg)
=/  repo  ;;(path +>.q.arg)
=/  branch=cord  'master'
~&  >  "Building desk {<desk>} from https://github.com{<repo>}:{(trip branch)}."
=/  tid  `cord`(cat 3 'strand_' (scot %uv (sham %retrieve-gh eny.bowl)))
;<  ~       bind:m  %-  watch-our:strandio
                    :*  /awaiting/[tid]
                        %spider
                        /thread-result/[tid]
                    ==
;<  ~       bind:m  %-  poke-our:strandio
                    :*  %spider
                        %spider-start
                        !>  :*  `tid.bowl
                                `tid
                                byk.bowl(r da+now.bowl)
                                %retrieve-gh
                                !>(`[repo branch])
                    ==      ==
;<  =cage   bind:m  (take-fact:strandio /awaiting/[tid])
;<  ~       bind:m  (take-kick:strandio /awaiting/[tid])
?:  =(%thread-fail p.cage)
  (strand-fail:strandio !<([term tang] q.cage))
?>  ?=(%thread-done p.cage)
~&  >  "Successfully retrieved."
~&  >>  (met 3 (jam q.q.cage))
:: ~&  >>>  q.q.cage
:: =/  sob  !<(soba:clay [%noun q.q.cage])
=/  sob  ;;(soba:clay q.q.cage)
~&  >  "Merging files into new desk {<desk>}."
::  Does the desk exist?
?.  (~(has in .^((set ^desk) %cd /=//=)) desk)
  ;<  ~  bind:m  (send-raw-card:strandio [%pass /deployer-commit %arvo %c %merg desk our.bowl desk da+now.bowl %init])
  ;<  ~  bind:m  (send-raw-card:strandio [%pass /deployer-commit %arvo %c %info desk %& sob])
  ;<  ~  bind:m  (sleep:strandio ~s0)  ::  wait for merge to complete
  ~&  >  "Desk {<desk>} created."
  (pure:m !>(desk))
;<  ps=(list path)  bind:m  (scry:strandio (list path) /ct/[desk])
=/  ins=(set path)  (silt (turn sob head))
=/  dif=(set path)  (~(dif in (silt ps)) ins)
~&  dif
=/  sob  (weld (turn ~(tap by dif) |=(p=path [p %del ~])) sob)
;<  now=@da  bind:m  get-time:strandio
;<  =ship    bind:m  get-our:strandio
;<  ~  bind:m  (send-raw-card:strandio [%pass /deployer-commit %arvo %c %info desk %& sob])
;<  ~  bind:m  (sleep:strandio ~s0)  ::  wait for merge to complete
~&  >  "Desk {<desk>} updated."
(pure:m !>(desk))
