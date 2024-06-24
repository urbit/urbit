/-  spider
/+  strandio, jammer=desk-jam
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ desks=(list desk)] arg)
=?  desks  =(~ desks)
  :~  %base
      %garden
      %landscape
      %webterm
      %bitcoin
  ==
^-  form:m
::
;<  =bowl:spider  bind:m  get-bowl:strandio
;<  ~  bind:m
  %-  send-raw-card:strandio
  =-  [%pass /tmp-desks %arvo %c %info %base %& -]
  ^-  soba:clay
  %+  turn  desks
  |=  =desk
  :*  /tmp/[desk]/jam  %ins  %jam  %noun
      (jam-desk:jammer our.bowl desk now.bowl tick.bowl)
  ==
::
(pure:m !>(ok=&))
