!:
::          %wol, the shell.  This file is in the public domain.
::
|%
++  berg                                            ::  repl/shell
  |=  who=flag                                      ::  per identity
  =+  nub=`vase`!>(+>)                              ::  system + libraries
  =+  hit=[p=0 q=*(list tape)]                      ::  source history
  =+  sur=[p=0 q=*(qeu vase)]                       ::  result history
  =+  haf=[p=0 q=*tape]                             ::  partial input
  =+  hox=~(rent co ~ %p who)                       ::  identity string
  =+  way=*(map ,@ta vase)                          ::  environment
  |_  [now=@da sky=_|+(* *(unit))]
  ++  cath                                          ::  default path
    ^-  path
    [hox ~(rent co ~ %da now) %cx (hoof stub) ~]
  ::
  ++  chad                                          ::  apply command
    |=  [pez=(unit ,@ta) gen=gene]
    ^-  [p=(list card) q=_+>]
    =+  ten=(slab =([~ %how] pez) [lube gen] sky)
    ?-  -.ten
      %0  (chaw pez p.ten)
      %1  [(chaf p.ten) +>.$]
      %2  [[[%crap p.ten] ~] +>.$]
    ==
  ::
  ++  chaf                                          ::  missing error
    |=  lap=(list)
    ^-  (list card)
    :-  [%tell %3 %leaf "<not found>"]
    %+  turn
      lap
    |=(a=* [%tell %2 (dish:ut [~ %path] (path a))])
  ::
  ++  chaw                                          ::  apply result
    |=  [pez=(unit ,@ta) vax=vase]
    ^-  [p=(list card) q=_+>]
    ?~  pez
      =+  ham=~(dole ut p.vax)
      :-  :~  ::  [%tell %0 (dial:ut ham)]
              [%tell %1 (dish:ut ham q.vax)]
          ==
      (stay vax)
    ?:  =(%how u.pez)
      :_(+> [[%tell %1 (dial:ut ~(dole ut p.vax))] ~])
    ?:  =(%set u.pez) 
      ?.  (~(nest ut [%cell %noun %noun]) | p.vax)
        [none +>.$]
      =+  var=(love '@tas' (slot 2 vax))
      ?~  var
        [none +>.$]
      [~ +>.$(way (~(put by way) (,@tas u.var) (slot 3 vax)))]
    :_  +>.$
    =+  zup=[[%cell [%cube u.pez [%atom %tas]] p.vax] [u.pez q.vax]]
    =+  dax=(love '*card' zup)
    ?~  dax
      [[%tell %3 [%leaf "<invalid command>"]] ~]
    [((hard card) u.dax) ~]
  :: 
  ++  clam                                          ::  parse rule
    =+  vez=(vang | cath)
    ;~  pose 
      (cold [[~ %helo] [%dtsg %% 0]] vul)
      ;~  plug
        (stag ~ ;~(pfix col sym))
        ;~(pfix ace (stag %cltr (most ace wide:vez)))
      ==
      (stag ~ wide:vez)
    ==
  ::
  ++  clog                                          ::  parse virtually
    |*  sef=_rule
    |=  pet=tape
    ^-  ?(tone [3 p=tape])
    =+  ten=(mung [|=(a=tape ((full sef) [[1 1] a])) pet] sky)
    ?.  ?=(%0 -.ten)
      ten
    =+  duf=((hard edge) p.ten)
    ?^  q.duf
      [%0 p.u.q.duf]
    =+  ^=  end  |-  ^-  hair
        ?~  pet  [1 1]
        =+  har=$(pet t.pet)
        ?.(=(10 i.pet) [+(p.har) q.har] [p.har +(q.har)])
    ?:  =(end p.duf)
      [%3 pet] 
    =+  rux=~(rend co %many [%% %ud p.p.duf] [%% %ud q.p.duf] ~)
    [%2 [%lean `tank`[%leaf "<syntax error at {rux}>"]] ~]
  ::
  ++  hist                                          ::  add to sources
    |=  pet=tape  ^+  +>
    %_(+> p.hit +(p.hit), q.hit [pet q.hit])
  ::
  ++  knap                                          ::  apply event
    |=  fav=card
    ^-  [p=(list card) q=_+>]
    ?+    -.fav  [~ +>]
        %boot
      :-  [[%tell %0 %leaf "<reboot {lich}>"] prom ~]
      (berg who)
    ::
        %dire
      :-  ~
      %_(+> way (~(put by way) p.fav [(lewd 'dram') q.fav]))
    ::
        %file
      :-  ~
      %_(+> way (~(put by way) p.fav [[%atom %%] q.fav]))
    ::
        %helo
      [[prom ~] +>]
    ::
        %line
      =+  pet=(rip 3 p.fav)
      =>  .(p.hit +(p.hit), q.hit [pet q.hit])
      =+  cug=((clog clam) pet)
      ?-  -.cug
        %0  (chad ((hard ,[p=(unit ,@ta) q=gene]) p.cug))
        %1  [(chaf p.cug) +>.$]
        %2  [[[%crap p.cug] ~] +>.$]
        %3  [[[%tell %3 %leaf "<syntax line>"] ~] +>.$]
      ==
    ==
  ::
  ++  lich                                          ::  name of shell
    ^-  tape
    ~(rend co [~ %p who])
  ::
  ++  lewd                                          ::  type by name
    |=  txt=@t
    ^-  type
    (~(play ut p.nub) (rash txt wide:vast))
  ::
  ++  love                                          ::  ^+ by name
    |=  [txt=@t vax=vase]
    ^-  (unit)
    ?.((~(nest ut (lewd txt)) | p.vax) ~ [~ q.vax])
  ::
  ++  lube                                          ::  make subject
    ^-  vase
    ;:  slop
      ;:  slop
        %+  slop
          [[%atom %da] now]
        [[%atom %ta] ~(rent co [~ %da now])]
      ::
        %+  slop
          [[%atom %p] who]
        [[%atom %ta] ~(rent co [~ %p who])]
      ::
        [(lewd '*(list ,@)') q.hit]
      ==
    ::
      =+  voy=(~(tap to q.sur) ~)
      |-  ^-  vase
      ?~(voy [[%atom %n] ~] (slop i.voy $(voy t.voy)))
    ::
      ?~  way
        nub
      %-  slop
      :_  nub
      |-  ^-  vase
      ?+  way  !!     ::  XX some inference weirdness here?
        [* ~ ~]  [[%face p.n.way p.q.n.way] q.q.n.way]
        [* ~ ^]  (slop $(r.way ~) $(way r.way))
        [* ^ ~]  (slop $(l.way ~) $(way l.way))
        [* ^ ^]  :(slop $(r.way ~, l.way ~) $(way l.way) $(way r.way))
      ==
    ==
  ::
  ++  none                                          ::  standard failure
    ^-  (list card)
    [[%tell %3 [%leaf "<invalid command>"]] ~]
  ::
  ++  prom                                          ::  normal prompt
    ^-  card
    [%prop & (rap 3 (weld lich ": "))]
  ::
  ++  stay                                          ::  add to results
    |=  vax=vase
    %_    +>
        sur
      ?:  =(16 p.sur)
        [16 (~(put to q:~(get to q.sur)) vax)]
      [+(p.sur) (~(put to q.sur) vax)]
    ==
  --
--
