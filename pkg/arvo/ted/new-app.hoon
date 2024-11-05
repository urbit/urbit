::  -new-app: creates a desk from example agent repo
::  Derived from pkova/deployer/ted/sync.hoon
::
/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
;<  =bowl:spider  bind:m  get-bowl:strandio
=/  desk-branch-dir  +<.q.arg
=/  repo  ;;(path +>.q.arg)
=/  [desk=term branch=cord dir=cord]
  ?+    desk-branch-dir  ~|(%weird-input !!)
      term
    [desk-branch-dir 'master' 'desk']
  ::
      [desk=term branch=cord]
    [desk branch 'desk']:desk-branch-dir
  ::
      [desk=term branch=cord dir=cord]
    [desk branch dir]:desk-branch-dir
  ==
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
                                !>(`[repo branch dir])
                    ==      ==
;<  =cage   bind:m  (take-fact:strandio /awaiting/[tid])
;<  ~       bind:m  (take-kick:strandio /awaiting/[tid])
?:  =(%thread-fail p.cage)
  (strand-fail:strandio !<([term tang] q.cage))
?>  ?=(%thread-done p.cage)
?~  q.q.cage
  ~&  >>>  "No files found at /{(trip dir)} in repo."
  (strand-fail:strand %no-desk-in-repo ~)
~&  >  "Successfully retrieved files."
=/  fil  ;;((list (pair path (pair @ud @))) q.q.cage)
=|  sob=soba:clay
=.  sob
  |-  ^-  soba:clay
  ?~  fil  sob
  $(fil t.fil, sob [[`path`p.i.fil `miso:clay`[%ins %mime !>([/ q.i.fil])]] sob])
~&  >  "Merging {<(lent sob)>} files into new desk {<desk>}."
::  Does the desk exist?
=/  desks  .^((set ^desk) %cd /(scot %p our.bowl)//(scot %da now.bowl))
?.  (~(has in desks) desk)
  ~&  >  "Desk {<desk>} does not exist yet; creating."
  ;<  ~  bind:m  (send-raw-card:strandio [%pass /new-app %arvo %c %merg desk our.bowl %base da+now.bowl %init])
  ~&  >  "Desk {<desk>} created."
  ::  Don't forget to clear out carryover from the source desk %base.
  ;<  ps=(list path)  bind:m  (scry:strandio (list path) /ct/base)
  =/  ins=(set path)  (silt (turn sob head))
  =/  dif=(set path)  (~(dif in (silt ps)) ins)
  =/  sob  (weld (turn ~(tap by dif) |=(p=path [p %del ~])) sob)
  ;<  now=@da  bind:m  get-time:strandio
  ;<  =ship    bind:m  get-our:strandio
  ;<  ~  bind:m  (send-raw-card:strandio [%pass /new-app %arvo %c %info desk %& sob])
  ;<  ~  bind:m  (sleep:strandio ~s0)  ::  wait for merge to complete
  ~&  >  "Desk {<desk>} populated."
  (pure:m !>(desk))
~&  >  "Desk {<desk>} already exists; updating."
;<  ps=(list path)  bind:m  (scry:strandio (list path) /ct/[desk])
=/  ins=(set path)  (silt (turn sob head))
=/  dif=(set path)  (~(dif in (silt ps)) ins)
=/  sob  (weld (turn ~(tap by dif) |=(p=path [p %del ~])) sob)
;<  now=@da  bind:m  get-time:strandio
;<  =ship    bind:m  get-our:strandio
;<  ~  bind:m  (send-raw-card:strandio [%pass /new-app %arvo %c %info desk %& sob])
;<  ~  bind:m  (sleep:strandio ~s0)  ::  wait for merge to complete
~&  >  "Desk {<desk>} updated."
(pure:m !>(desk))
