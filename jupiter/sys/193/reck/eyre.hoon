!:
::          %vay, main loop.   This file is in the public domain.
::
=<  |%
    ++  peek  
      |=  [now=@da hap=path]
      ^-  (unit)
      ?~  hap  ~
      =+  fod=(slay i.hap)
      ?:  ?=([%% ~ %p @] fod)
        (~(beck is now) q.p.u.fod t.hap)
      ?.  ?=([%% ~ %tas @] fod)  ~
      (~(dear is now) q.p.u.fod t.hap)
    ::
    ++  poke  
      |=  [now=@da ovo=*]
      =>  .(ovo ((hard ovum) ovo))
      ::  ~&  [%ovum ovo]
      ^-  [p=(list ovum) q=_+>]
      =^  zef  sys
        (~(howl is now) ~ [[%eyre ~] p.ovo ~] q.ovo)
      [zef +>.$]
    --
=>  |%
    ++  game
      $:  ^=  arvo                                          ::  network and PKI
        $:  gel=_*alga
        ==
          ^=  bede                                          ::  shell
        $:  maw=(map lord _*berg)
        ==
          ^=  cary                                          ::  filesystem
        $:  duw=_clay
        ==
          ^=  dill                                          ::  http server
        $:  lyd=_lidl
        ==
          ^=  eyre                                          ::  i/o
        $:  gem=(map lord chum)                             ::  hashed passcodes
            liv=(map lord (list tube))                      ::  live consoles
            rev=(map tube lord)                             ::  identities
            sak=(map lord (list ,[p=tube q=prod]))          ::  prompt stacks
            tuh=(map tube logo)                             ::  http sessions
            tyv=(map logo cred)                             ::  credentials
        ==
      == 
    --
=+  sys=*game
=.  gel.arvo.sys  $:alga
=.  duw.cary.sys  clay
=.  lyd.dill.sys  lidl
|%
++  is
  |_  now=@da 
  ++  si
    |%  
    ++  arv
      |=  [pex=path mov=move]
      ^-  [(list move) game]
      =+  yub=(knap:gel.arvo.sys now r.mov)
      :-  (turn p.yub |=(a=card [p.mov q.mov a]))
      sys(gel.arvo q.yub)
    ::
    ++  bed
      |=  [pex=path mov=move]
      ^-  [(list move) game]
      ?>  ?=(^ p.mov)
      =+  ^=  beg  ^+  *berg
          =+  beg=(~(get by maw.bede.sys) u.p.mov)
          ?^(beg u.beg (berg u.p.mov))
      =+  yub=(leap:(beg now |=(a=* (beck u.p.mov (path a)))) pex q.mov r.mov)
      :-  p.yub
      sys(maw.bede (~(put by maw.bede.sys) u.p.mov q.yub))
    ::
    ++  car
      |=  [pex=path mov=move]
      ^-  [(list move) game]
      ?>  ?=(^ p.mov)
      =+  yub=(drip:duw.cary.sys now u.p.mov r.mov)
      :-  (turn p.yub |=(a=card [p.mov q.mov a]))
      sys(duw.cary q.yub)
    ::
    ++  dyl
      |=  [pex=path mov=move]
      ^-  [(list move) game]
      =^  yub  lyd.dill.sys  
        (~(leap lyd.dill.sys now (shax now)) mov)
      [p.yub sys]
    --
  ::
  ++  auth                                                  ::  match password
    |=  [our=lord cof=chum]
    ^-  ?
    =+  fup=(~(get by gem.eyre.sys) our)
    ?^  fup
      =(cof u.fup)
    =+  gys=(~(us go ton.fox.gel.arvo.sys) our)
    ?&(?=(^ gys) =(cof (shak our pac:ex:q:sen:u.gys)))
  ::
  ++  beck                                                  ::  namespace
    |=  [our=lord hap=path]
    ^-  (unit)
    ::  ~&  [%beck our hap]
    ?.  ?=([@ @ @ *] hap)  ~
    =+  :*  fal=(slay i.hap) 
            ved=(slay i.t.hap) 
            hyr=(slay i.t.t.hap)
            tyl=t.t.t.hap
        ==
    ?.  ?=([~ %% %p @] fal)  ~
    ?.  ?=([~ %% %tas @] hyr)  ~
    =+  [pef=(end 3 1 q.p.u.hyr) rem=(rsh 3 1 q.p.u.hyr)]
    ?+    pef  ~
        %a                                                  ::  arvo
      ?.  =(0 rem)  ~
      ?+    ved  ~
          [~ %% %ud @]
        (perm:gel.arvo.sys our q.p.u.fal q.p.u.ved tyl)
      ::
          [~ %% %da @]
        ?.  =(now q.p.u.ved)  ~
        (temp:gel.arvo.sys our q.p.u.fal tyl)
      ==
    ::
        %c                                                  ::  cary
      ?.  ?=(?(%z %y %x %w) rem)  ~
      ?.  ?=([~ %% ?(%ud %da %tas) @] ved)  ~
      (scry:duw.cary.sys rem our u.ved tyl)
    ==
  ::
  ++  dear                                                  ::  global vision
    |=  [mol=@tas hap=path]
    ^-  (unit)
    ?+    mol  ~
        %eyre
      ?+    hap  ~
          [%prod *]  ^-  (unit ,[p=prom q=@tas])
        =+  yup=(~(get by rev.eyre.sys) `tube`[t.hap ~])
        ?~  yup
          [~ & '# ']
        =+  zib=(~(get by sak.eyre.sys) u.yup)
        ?:  |(?=(~ zib) ?=(~ u.zib))
          [~ [& (rap 3 "{~(rend co ~ %p u.yup)}> ")]]
        [~ p.q.i.u.zib (rap 3 q.q.i.u.zib)]
      ==
    ==
  ::
  ++  grit                                                  ::  cause privilege
    |=  cul=tube
    ^-  ?(%gold %iron %lead)
    ?~  cul
      %lead
    ?~  t.cul
      ?:  ?=([%gold *] i.cul)  %gold
      ?:  ?=([%iron *] i.cul)  %iron
      %lead
    $(cul t.cul)
  ::
  ++  howl                                                  ::  handle event 
    |=  mov=move
    =+  [mor=`(list move)`[mov ~] out=`(list ovum)`~]
    |-  ^-  [p=(list ovum) q=game]
    ?~  mor
      [(flop out) sys]
    =>  %_(. mov i.mor)
    ?>  ?=(^ q.mov)           ::  at least one path
    ?>  ?=(^ i.q.mov)         ::  at least prefix
    =+  pex=t.i.q.mov
    ?+    i.i.q.mov  !!
        %arvo
      =^  fez  sys
        (arv:si pex [p.mov t.q.mov r.mov])
      $(mor (weld fez t.mor))
    ::
        %bede
      =^  fez  sys
        (bed:si pex [p.mov t.q.mov r.mov])
      $(mor (weld fez t.mor))
    ::
        %cary
      =^  fez  sys
        (car:si pex [p.mov t.q.mov r.mov])
      $(mor (weld fez t.mor))
    ::
        %dill
      =^  fez  sys
        (dyl:si pex [p.mov t.q.mov r.mov])
      $(mor (weld fez t.mor))
    ::
        %eyre
      =.  p.mov  ?^(p.mov p.mov (~(get by rev.eyre.sys) t.q.mov))
      =+  rer=|=(a=@tas ^$(mor [[p.mov [[a ~] q.mov] r.mov] t.mor]))
      =+  giv=|.(^$(mor t.mor, out [[i.t.q.mov r.mov] out]))
      =+  mel=(grit t.q.mov)
      =+  ^=  lov
          |=  a=lord 
          ^$(mor t.mor, rev.eyre.sys (~(put by rev.eyre.sys) t.q.mov a))
      ?.  ?|  ?=(^ p.mov) 
              ?=(?(%cash %crap %edit %hear %logn %make %talk %warn) -.r.mov)
          ==
        ~&([%eyre-illegal mov] !!)
      ?-    -.r.mov
          %bbye  !!
          %boot  !!
          %cash  (rer %arvo)
          %crap  (giv)
          %dire  (rer %bede)
          %edit
        %=    $
            mor
          :_  t.mor
          :+  ?^  p.mov  
                ?>(=(u.p.mov p.r.mov) p.mov)
              ?>(=(%gold mel) [~ p.r.mov])
            [[%cary ~] q.mov]
          r.mov
        ==
      ::
          %file  (rer %bede)
          %hear  (rer %arvo)
          %helo  !!
          %init  (rer %cary)
          %junk  (rer %arvo)
          %line
        ?>  ?=(^ p.mov)
        =+  veh=(~(get by sak.eyre.sys) u.p.mov)
        ?:  |(?=(~ veh) ?=(~ u.veh))
          (rer %bede)
        %=    $
            mor           [[p.mov p.i.u.veh r.mov] t.mor]
            sak.eyre.sys
          ?~  t.u.veh
            (~(del by sak.eyre.sys) u.p.mov)
          (~(put by sak.eyre.sys) u.p.mov t.u.veh)
        ==
      ::
          %load  (giv) 
          %logn 
        ?>  !=(%lead mel)
        ?>  (auth p.r.mov q.r.mov)
        (lov p.r.mov)
      ::
          %logp
        ?>  =(%gold mel)
        ~&  [%logp p.r.mov]
        (lov p.r.mov)
      ::
          %loot  (giv)
          %love   !!
          %make  (rer %arvo)
          %mine  (rer %bede)
          %pace  !!
          %pour  (giv)
          %prop 
        ?>  ?=(^ p.mov)
        ?>  ?=(^ t.q.mov)
        =+  veh=(~(get by sak.eyre.sys) u.p.mov)
        %=    $
            mor  t.mor
            sak.eyre.sys 
          %+  ~(put by sak.eyre.sys)
            u.p.mov
          [[t.q.mov p.r.mov] ?~(veh ~ u.veh)]
        ==
      ::
          %pump  !!
          %resp  !!
          %save  (giv)
          %send  (giv)
          %ship  (rer %cary)
          %thin  !!
          %this  !!
          %sync  !!
          %talk  (giv) 
          %text  (giv)
          %tory  !!
          %warn  (giv) 
          %word  !!
      ==
    ==
  --
  ::
  ++  rand
    |=  [wid=@ ent=*]
    ^-  [p=@ q=game]
    =+  guh=(rand:gel.arvo.sys wid ent)
    [p.guh sys(gel.arvo q.guh)]
--
