/-  spider, hark=hark-store
/+  strandio
=>
|% 
+$  notes   (map bin:hark @ud)
++  strand  strand:spider
++  fail    strand-fail:strand
++  poke  poke:strandio
++  poke-our   poke-our:strandio
++  do-run
  |=   n=notes
  =/  m  (strand ,notes)
  ^-  form:m
  =/  ns=(list (pair bin:hark @ud))
    ~(tap by n)
  |-  =*  loop  $
  ^-  form:m
  ~&  n/n
  ~&  ns/ns
  ?~  ns  (pure:m n)
  ?:  =(0 q.i.ns)
    =.  n  (~(del by n) p.i.ns)
    loop(ns t.ns)
  ;<  ~  bind:m  (add-note i.ns)
  =.  q.i.ns  (dec q.i.ns)
  =.  n  (~(put by n) [p q]:i.ns)
  loop(ns t.ns)
::
++  add-note
  |=  [=bin:hark count=@ud]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl:strandio
  =/  =body:hark
    :*  ~[text/(crip "Title: {<bin>}")]
        ~[text/(crip "Content: {<count>}")]
        now.bowl
        /
        /
    ==
  =/  =cage
    :-  %hark-action
    !>  ^-  action:hark
    [%add-note bin body]
  ::
  (poke-our %hark-store cage)
++  default-notes
  %-  ~(gas by *notes)
  =/  =place:hark
    [%garden /place]
  ^-  (list [bin:hark @ud])
  :~  [/foo^place 1]
      [/bar^place 2]
      [/baz^place 3]
  ==
--
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
:: =+  !<(m-notes=(unit notes) arg)
=/  =notes  default-notes :: (fall m-notes default-notes)
|-  =*  loop  $
?:  =(~ notes)
  (pure:m *vase)
;<  new-notes=_notes  bind:m 
  (do-run notes)
;<  ~  bind:m
  (sleep:strandio ~s5)
loop(notes new-notes)
