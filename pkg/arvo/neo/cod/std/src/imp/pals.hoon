/@  pal-diff
/@  pals-diff
/@  pals-action
/@  pal-type=pal
=> 
|%
++  kind   ?(%target %leeche %mutual)
++  card  card:neo
++  get-kind
  |=  [=bowl:neo =ship]
  ^-  (unit kind) 
  =/  =pith  #/pals/[p/ship]
  ?~  kid=(~(get of:neo kids.bowl) pith)
    ~
  =+  !<(child=pal-type q.pail.u.kid)
  `type.child
++  her-hey
  |=  kin=(unit kind)
  ^-  kind
  ?~  kin
    %leeche
  ?:  ?=(%leeche u.kin)
    %leeche
  %mutual
++  her-bye
  |=  kin=kind
  ^-  (unit kind)
  ?:  ?=(%leeche kin)
    ~
  `%target
::
++  add-pal
  |=  [kind=?(%target %leeche %mutual) =bowl:neo =ship]
  ^-  card
  =/  init=pal-type  [ship kind ~]
  [(welp here.bowl (pave:neo /pals/(scot %p ship))) %make %pal `pal/!>(init) ~]
++  tell-pal
  |=  [=ship diff=pals-diff]
  ^-  card
  =/  were=pith  (pave:neo /(scot %p ship)/pals)
  [were %poke %pals-diff !>(diff)]

--
^-  kook:neo
|%
++  state  pro/%sig
++  poke  (sy %pals-diff ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [&/%pals |/%p |] :: /pals/[who=@p]
      [pro/%pal (sy %pal-diff ~)]   :: state = %pal, diff = %pal-diff
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =saga:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?+    stud  !!
        %pals-diff
      =+  !<(poke=pals-diff vax)
      :: ?.  ;;(? +:(~(gut by deps.bowl) %open [*pith &]))
       :: ~&(dropping-poke/poke !>(sta))
      :_  q.saga
      (add-pal %leeche bowl ship.src.bowl)^~
    ::
        %pals-action
      =+  !<(poke=pals-action vax)
      =/  diff=pals-diff
        ?:  ?=(%meet -.poke)
          hey/~
        bye/~
      :_  q.saga
      :~  (tell-pal ship.poke diff)
          (add-pal %target bowl ship.poke)
      ==
    ==
  ++  init
    |=  old=(unit pail:neo)
    `sig/!>(~)
  --
--
