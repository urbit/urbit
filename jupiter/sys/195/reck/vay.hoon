!:
::          %vay, main loop.   This file is in the public domain.
::
=<  |%
    ++  peek  
      |=  [now=@da hap=path]
      ^-  (unit)
      ?~  hap  ~
      =+  fod=(slay i.hap)
      ?.  ?=([%% ~ %p @] fod)  ~
      (~(beck is now) q.p.u.fod t.hap)
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
          ^=  bach                                          ::  shell
        $:  maw=(map flag _*berg)
        ==
          ^=  cary                                          ::  filesystem
        $:  duw=_clay
        ==
          ^=  eyre                                          ::  i/o
        $:  gem=(map flag chum)                             ::  hashed passcodes
            liv=(map flag (list caul))                      ::  live sessions
            rev=(map caul flag)                             ::  identities
        ==
      == 
    --
=+  sys=*game
=.  gel.arvo.sys  $:alga
=.  duw.cary.sys  clay
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
    ++  bac
      |=  [pex=path mov=move]
      ^-  [(list move) game]
      ?>  ?=(^ p.mov)
      =+  ^=  beg  ^+  *berg
          =+  beg=(~(get by maw.bach.sys) u.p.mov)
          ?^(beg u.beg (berg u.p.mov))
      =+  yub=(~(knap beg [now |=(a=* (beck u.p.mov (path a)))]) r.mov)
      :-  (turn p.yub |=(a=card [p.mov q.mov a]))
      sys(maw.bach (~(put by maw.bach.sys) u.p.mov q.yub))
    ::
    ++  car
      |=  [pex=path mov=move]
      ^-  [(list move) game]
      ?>  ?=(^ p.mov)
      =+  yub=(drip:duw.cary.sys now u.p.mov r.mov)
      :-  (turn p.yub |=(a=card [p.mov q.mov a]))
      sys(duw.cary q.yub)
    --
  ::
  ++  auth                                                  ::  match password
    |=  [our=flag cof=chum]
    ^-  ?
    =+  fup=(~(get by gem.eyre.sys) our)
    ?^  fup
      =(cof u.fup)
    =+  gys=(~(us go ton.fox.gel.arvo.sys) our)
    ?&(?=(^ gys) =(cof (shak our pac:ex:q:sen:u.gys)))
  ::
  ++  beck                                                  ::  namespace
    |=  [our=flag hap=path]
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
          [~ %% %ud @]                                      ::  permanent
        (perm:gel.arvo.sys our q.p.u.fal q.p.u.ved tyl)
      ::
          [~ %% %da @]                                      ::  transient
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
  ++  grit                                                  ::  cause privilege
    |=  cul=caul
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
        %bach
      =^  fez  sys
        (bac:si pex [p.mov t.q.mov r.mov])
      $(mor (weld fez t.mor))
    ::
        %cary
      =^  fez  sys
        (car:si pex [p.mov t.q.mov r.mov])
      $(mor (weld fez t.mor))
    ::
        %eyre
      =.  p.mov  ?^(p.mov p.mov (~(get by rev.eyre.sys) t.q.mov))
      =+  rer=|=(a=@tas ^$(mor [[p.mov [[a ~] q.mov] r.mov] t.mor]))
      =+  giv=|.(^$(mor t.mor, out [[i.t.q.mov r.mov] out]))
      =+  mel=(grit t.q.mov)
      =+  ^=  liv
          |=  a=flag 
          ^$(mor t.mor, rev.eyre.sys (~(put by rev.eyre.sys) t.q.mov a))
      ?.  ?|  ?=(^ p.mov) 
              ?=(?(%cash %crap %edit %hear %logn %make %tell) -.r.mov)
          ==
        ~&([%eyre-illegal mov] !!)
      ?-    -.r.mov
          %bbye  !!
          %boot  !!
          %cash  (rer %arvo)
          %crap  (giv)
          %dire  (rer %bach)
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
          %file  (rer %bach)
          %hear  (rer %arvo)
          %helo  !!
          %init  (rer %cary)
          %junk  (rer %arvo)
          %line  (rer %bach)
          %load  (giv) 
          %logn 
        ?>  !=(%lead mel)
        ?>  (auth p.r.mov q.r.mov)
        (liv p.r.mov)
      ::
          %logp
        ?>  =(%gold mel)
        (liv p.r.mov)
      ::
          %loot  (giv)
          %make  (rer %arvo)
          %pace  !!
          %pour  (giv)
          %prop  (giv)
          %pump  !!
          %save  (giv)
          %send  (giv)
          %ship  (rer %cary)
          %sync  !!
          %task  !!
          %tell  (giv) 
          %text  (giv)
          %tory  !!
          %word  !!
      ==
    ==
  --
--
