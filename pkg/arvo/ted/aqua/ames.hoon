::  This needs a better SDN solution.  Every ship should have an IP
::  address, and we should eventually test changing those IP
::  addresses.
::
::  For now, we broadcast every packet to every ship and rely on them
::  to drop them.
::
/-  aquarium, spider
/+  aqua-vane-thread
=,  aquarium
|%
+$  fiefs  (map ship fiefs-result:jael)
--
::
=|  fez=fiefs
::
|%
++  emit-aqua-events
  |=  [our=ship aes=(list aqua-event)]
  ^-  (list card:agent:gall)
  [%pass /aqua-events %agent [our %aqua] %poke %aqua-events !>(aes)]~
::
++  handle-fief
  |=  [our=ship who=ship way=wire %fief =fiefs-result:jael]
  ^-  (quip card:agent:gall fiefs)
  :-  ~
  %+  ~(put by fez)  who
  (~(uni by (~(gut by fez) who ~)) fiefs-result)
::
++  handle-restore
  |=  [our=ship who=@p]
  ^-  (quip card:agent:gall fiefs)
  :_  fez
  %+  emit-aqua-events  our
  [%event who [/a/newt/0v1n.2m9vh %born ~]]~
::
++  handle-send
  =,  ames
  |=  [our=ship now=@da sndr=@p way=wire %send lan=lane pac=@]
  ^-  (quip card:agent:gall fiefs)
  :_  fez
  =/  rcvr=(unit @p)  (lane-to-ship sndr lan)
  ?~  rcvr
    ~&([%aqua %ames %error "can't resolve lane"] ~)
  =/  hear-lane  (ship-to-lane sndr)
  =/  =shot      (sift-shot pac)
  ?:  &(!sam.shot req.shot)  :: is fine request
    =/  [%0 =peep]  (sift-wail `@ux`content.shot)
    %+  emit-aqua-events  our
    :_  ~
    :-  %read
    [[[u.rcvr rcvr-tick.shot] path.peep] [hear-lane sndr-tick.shot] num.peep]
  %+  emit-aqua-events  our
  [%event u.rcvr /a/newt/0v1n.2m9vh %hear hear-lane pac]~
::  +lane-to-ship: decode a ship from an aqua lane
::
::    Special-case some comets, since their addresses doesn't fit into a lane.
::
++  lane-to-ship
  |=  [sndr=@p =lane:ames]
  ^-  (unit ship)
  ?-    -.lane
      %&
    ?.  =(%pawn (clan:title p.lane))
      (some p.lane)
    =/  =fiefs-result:jael  (~(gut by fez) sndr ~)
    ?~  got=(~(gut by fiefs-result) p.lane *(unit fief))
      ~
    ?-    -.u.got
        %is  ~
        %turf
      |-  ^-  (unit ship)
      ?~  p.u.got  ~
      ?^  tuf=(~(get by turfs) i.p.u.got)
        tuf
      $(p.u.got t.p.u.got)
    ::
        %if
      ?.  =(0xdead.beef p.u.got)  ~
      ?.  (lth q.u.got 12)  ~
      $(lane [%| (cat 5 p.u.got q.u.got)])
    ==
  ::
      %|
    ?:  ?&  =(0xdead.beef (end 5 p.lane))
            (lth (rsh 5 p.lane) 12)
        ==
      (some (snag (rsh 5 p.lane) comets))
    (some `@p``@`p.lane)
  ==
::  +ship-to-lane: encode a lane to look like it came from .ship
::
::    Never shows up as a galaxy, because Vere wouldn't know that either.
::    Special-case a list of comets, since its address doesn't fit into a lane.
::
++  ship-to-lane
  |=  =ship
  ^-  lane:ames
  :-  %|
  ^-  address:ames  ^-  @
  =/  index=(unit @ud)  (find ~[ship] comets)
  ?~  index
    ship
  (cat 2 0xdead.beef u.index)
::  +comets: list of hard-coded comets
::
++  comets
  ^~  ^-  (list @p)
  :~  :: %c suite, marbud, 0xdead.beef.cafe tweak
      ~fasteg-dinhet-malrum-ransub--hocduc-digtev-radsut-marbud
      ~daldyl-nildem-dispec-tilryx--dondus-dirmet-tintyl-marbud
      ~dansyr-ponbec-tocfel-laddux--socnut-nisnyx-dinsut-marbud
      :: %b suite, marbud
      ~harrep-podpec-torsut-docnyx--mopsyx-fosdus-ladpen-marbud
      ~liblyn-togrut-tabwel-hodbet--dovbex-parryt-mirbyt-marbud
      ~hidreb-naptev-banben-bicrup--massup-dantus-fodwet-marbud
      :: %c suite, mardev, 0xdead.beef.cafe tweak
      ~molpyx-novtyc-wortyc-noswyd--taltyv-loplev-dabwen-mardev
      ~fosnys-noctyd-talfyl-borryl--davhus-disbyn-fotnec-mardev
      ~tonmep-tabrux-rinbep-firmur--silmex-saldef-pasfer-mardev
      :: %b suite, mardev
      ~holwyx-ramped-tognet-barsyn--navler-ronmeg-topbex-mardev
      ~hacmet-doslyr-narhut-tiptec--micbyl-motnev-worsyn-mardev
      ~ribmut-nopdul-minmet-pardeg--wisfex-rosfus-fogsyn-mardev
  ==
:: +turfs: map from domain to comet
::
++  turfs
  ^~  ^-  (map turf @p)
  %-  ~(gas by *(map turf @p))
  ^-  (list [turf @p])
  =-  (zip - comets)
  ^-  (list turf)
  :~  /marbud/fasteg  /marbud/daldyl  /marbud/dansyr
      /marbud/harrep  /marbud/liblyn  /marbud/hidreb
      /mardev/molpyx  /mardev/fosnys  /mardev/tonmep
      /mardev/holwyx  /mardev/hacmet  /mardev/ribmut
  ==
::
::  +zip: combine two equally long lists into one list of cells
::
++  zip
    |*  [a=(list) b=(list)]
    ^-  (list [_?>(?=(^ a) i.a) _?>(?=(^ b) i.b)])
    =|  out=(list [_?>(?=(^ a) i.a) _?>(?=(^ b) i.b)])
    ?>  =((lent a) (lent b))
    |-
    ?~  a  (flop out)
    ?~  b  (flop out)
    $(out [[i.a i.b] out], a t.a, b t.b)
--
::
%+  aqua-vane-thread  ~[%fief %restore %send]
|_  =bowl:spider
+*  this  .
++  handle-unix-effect
  |=  [who=@p ue=unix-effect]
  ^-  (quip card:agent:gall _this)
  =^  cards  fez
    ?+  -.q.ue  [~ fez]
      %fief     (handle-fief our.bowl who ue)
      %restore  (handle-restore our.bowl who)
      %send     (handle-send our.bowl now.bowl who ue)
    ==
  [cards this]
::
++  handle-arvo-response  |=(* !!)
--
