/-  *sss
::
|%
++  mk-subs  |*  [=(lake) paths=mold]  -:+6:(da lake paths)
++  mk-pubs  |*  [=(lake) paths=mold]  -:+6:(du lake paths)
++  mk-mar
  |*  =(lake)
  |_  =(response:poke lake *)
  ++  grow
    |%
    ++  noun  response
    --
  ++  grab
    |%
    ++  noun  (response:poke lake *)
    --
  ++  grad  %noun
  --
++  lake-mark
  |=  =mark
  ^+  mark
  ?>  =('sss-' (end [3 3] mark)) 
  (cut 3 [4 (met 3 mark)] mark)
::
++  fled                                     ::  Like +sped but head is a path.
  |=  vax=vase
  ^-  vase
  :_  q.vax
  %-  ~(play ut p.vax)
  =-  [%wtgr [%wtts - [%& 2]~] [%$ 1]]
  =/  pax  ~|  %need-path  ;;(path -.q.vax)
  |-  ^-  spec
  ?~  pax  [%base %null]
  [%bccl ~[[%leaf %ta -.pax] $(pax +.pax)]]
::
++  zoom  |=  =noun  ~|  %path-none  $/sss/;;(path noun)
++  da
  |*  [=(lake) paths=mold]
  =>
    |%
    +$  flow
      $:  =aeon
          rok=[=aeon fail=_| =rock:lake]
          wav=((mop aeon wave:lake) lte)
      ==
    --
  |_  [sub=(map [ship dude paths] flow) =bowl:gall result-type=type on-rock-type=type]
  +*  wav     ((on aeon wave:lake) lte)
  +$  from    (on-rock:poke lake paths)
  +$  into    (response:poke lake paths)
  +$  result  (request:poke paths)
  ++  pine
    |=  [=what =ship =dude path=paths]
    ^-  card:agent:gall
    :*  %pass   (zoom request/pine/ship^dude^what^path)
        %agent  [ship dude]
        %poke   %sss-to-pub  :-  result-type  ^-  result
        [path dap.bowl %pine what]
    ==
  ++  surf
    |=  [=ship =dude path=paths]
    (pine %wave ship dude path)
  ::
  ++  read
    ;;  (map [ship dude path] [fail=? rock:lake])
    %-  ~(run by sub)
    |=  =flow
    [fail rock]:rok.flow
  ::
  ++  chit
    |=  [[aeon=term ship=term dude=term path=paths] =sign:agent:gall]
    ^+  sub
    ?>  ?=(%poke-ack -.sign)
    ?~  p.sign  sub
    %+  ~(jab by sub)  [(slav %p ship) dude path]
    |=  =flow
    ?.  =(aeon.rok.flow (slav %ud aeon))  flow
    flow(fail.rok &)
  ::
  ++  behn
    |=  [ship=term =dude path=paths]
    ^-  (list card:agent:gall)
    =>  .(ship (slav %p ship))
    ?.  (~(has by sub) ship dude path)  ~
    ~[(pine %wave ship dude path)]
  ::
  ++  apply
    |=  res=(response:poke lake paths)
    ?~  ;;((soft path) path.res)  ~|  %need-path  !!  
    ?@  payload.res
      (pine-response res)
    (scry-response res)
  ::
  ++  pine-response
    |=  res=[path=paths from=dude =aeon =what]
    ^-  (quip card:agent:gall _sub)
    =*  current  [src.bowl from.res path.res]
    =/  =flow  (~(gut by sub) current *flow)
    :_  (~(put by sub) current flow(aeon (max aeon.flow aeon.res)))
    ?-    what.res
        %rock
      ?.  |((lth aeon.rok.flow aeon.res) =(aeon.res 0))  ~
      :~  :*  %pass   (zoom request/scry/(scot %p src.bowl)^from.res^what.res^(scot %ud aeon.res)^path.res)
              %agent  [src.bowl from.res]
              %poke   %sss-to-pub  :-  result-type  ^-  result
              [path.res dap.bowl %scry %rock aeon.res]
      ==  ==
    ::
        %wave
      =/  cards=(list card:agent:gall)
        :~  :*  %pass  (zoom behn/(scot %p src.bowl)^from.res^path.res)
                %arvo  %b  %wait  (add ~s10 now.bowl)
        ==  ==
      =?  cards  (gth aeon.res +(aeon.flow))  [(pine %rock current) cards]
      =?  cards  (gth aeon.res aeon.rok.flow)
        %+  weld  cards
        %+  turn  (gulf +(aeon.rok.flow) aeon.res)
        |=  =aeon
        ^-  card:agent:gall
        :*  %pass   (zoom request/scry/(scot %p src.bowl)^from.res^what.res^(scot %ud aeon)^path.res)
            %agent  [src.bowl from.res]
            %poke   %sss-to-pub  :-  result-type  ^-  result
            [path.res dap.bowl %scry %wave aeon]
        ==
      cards
    ==
  ::
  ++  scry-response
    |=  $:  path=paths
            =dude
            =aeon
            $%([what=%rock =rock:lake] [what=%wave =wave:lake])
        ==
    ^-  (quip card:agent:gall _sub)
    =*  current  [src.bowl dude path]
    =/  =flow  (~(gut by sub) current *flow)
    ?.  (lth aeon.rok.flow aeon)
      %.  `sub
      (slog leaf/"ignoring stale {<what>} at aeon {<aeon>}" ~)
    |^
    ?-    what
        %rock
      =.  wav.flow  (lot:wav wav.flow `aeon ~)
      =.  rok.flow  [aeon | rock]
      =.  aeon.flow  (max aeon aeon.flow)
      (swim ~)
    ::
        %wave
      ?:  =(aeon +(aeon.rok.flow))
        =.  rok.flow  [aeon | (wash:lake rock.rok.flow wave)]
        (swim `wave)
      `(~(put by sub) current flow(wav (put:wav wav.flow aeon wave)))
    ==
    ++  swim
      |=  wave=(unit wave:lake)
      ^-  (quip card:agent:gall _sub)
      =^  wave  wav.flow  (del:wav wav.flow +(aeon.rok.flow))
      ?^  wave
        =.  rok.flow  [+(aeon.rok.flow) | (wash:lake rock.rok.flow u.wave)]
        (swim wave)
      :_  (~(put by sub) current flow)
      :~  :*  %pass   (zoom on-rock/(scot %ud aeon.rok.flow)^(scot %p src.bowl)^dude^path)
              %agent  [our dap]:bowl
              %poke   %sss-on-rock  :-  on-rock-type  ^-  from
              [path src.bowl dude rock.rok.flow ^wave]
      ==  ==
    --
  --
++  du
  |*  [=(lake) paths=mold]
  =>
    |%
    +$  rule  [rocks=_1 waves=_5]
    +$  tide
      $:  rok=((mop aeon rock:lake) gte)
          wav=((mop aeon wave:lake) lte)
          rul=rule
      ==
    --
  |_  [pub=(map paths tide) =bowl:gall result-type=type]
  +*  rok  ((on aeon rock:lake) gte)
      wav  ((on aeon wave:lake) lte)
  ::
  +$  into    (request:poke paths)
  +$  result  (response:poke lake paths)
  ++  rule
    |=  [path=paths =^rule]
    ^+  pub
    %+  ~(jab by pub)  path
    |=  =tide
    (petrify tide(rul rule))
  ::
  ++  wipe
    |=  path=paths
    ^+  pub
    %+  ~(jab by pub)  path
    |=  =tide
    %*  .  (petrify tide(rul [0 0]))
      rul  rul.tide
    ==
  ++  give
    |=  [path=paths =wave:lake]
    ^+  pub
    ?~  ;;((soft ^path) path)  ~|  %need-path  !!
    %+  ~(put by pub)  path
    =/  =tide  (~(gut by pub) path *tide)
    =/  next=aeon
      .+  %+  max  
        (fall (bind (pry:rok rok.tide) head) 0)
      (fall (bind (ram:wav wav.tide) head) 0)
    =/  last=[=aeon =rock:lake]  (fall (pry:rok rok.tide) *[key val]:rok)
    =.  wav.tide  (put:wav wav.tide next wave)
    ?.  =(next (add aeon.last waves.rul.tide))  tide
    (petrify tide)
  ::
  ++  petrify
    ~&  %hmm
    |=  =tide
    ^+  tide
    =/  next=aeon
      %+  max  
        (fall (bind (pry:rok rok.tide) head) 0)
      (fall (bind (ram:wav wav.tide) head) 0)
    =/  last=[=aeon =rock:lake]  (fall (pry:rok rok.tide) *[key val]:rok)
    =.  rok.tide
      %+  gas:rok  +<-:gas:rok
      %-  tab:rok  :_  [~ +(rocks.rul.tide)]
      %^  put:rok  rok.tide  next
      %+  roll  (tab:wav wav.tide `aeon.last waves.rul.tide)
      |=  [[aeon =wave:lake] =_rock.last]
      (wash:lake rock wave)
    ~|  %rock-none
    tide(wav (lot:wav wav.tide (bind (ram:rok rok.tide) |=([r=@ *] (dec r))) ~))
  ::
  ++  read
    ;;  (map path rock:lake)
    %-  ~(run by pub)
    |=  =tide
    =<  rock
    =/  snap=[=aeon =rock:lake]  (fall (pry:rok rok.tide) *[key val]:rok)
    %+  roll  (tap:wav (lot:wav wav.tide `aeon.snap ~))
    |=  [[=aeon =wave:lake] =_snap]
    ?.  =(aeon +(aeon.snap))  snap
    [aeon (wash:lake rock.snap wave)]
  ::

::    %+  ~(put by pub)  path
::    =/  =tide  (~(gut by pub) path *tide)
::    =^  last  rok.tide  (pop:rok rok.tide)
::    =^  next  wav.tide
::      %^    (dip:wav ,[aeon rock:lake])
::          (lot:wav wav.tide `-.last ~)
::        last
::      |=  [[aeon =rock:lake] [=aeon =wave:lake]]
::      ^-  [(unit wave:lake) ? [^aeon rock:lake]]
::      [~ | aeon (wash:lake rock wave)]
::    tide(rok (put:rok +<-:put:rok next))
  ::
  ++  apply
    |=  req=(request:poke paths)
    ^-  card:agent:gall
    =*  mark  (cat 3 %sss- name:lake)
    =/  =tide  (~(gut by pub) path.req *tide)
    ?-    type.req
        %scry
      :*  %pass   (zoom response/scry/(scot %p src.bowl)^from.req^what.req^(scot %ud aeon.req)^path.req)
          %agent  [src.bowl from.req]
          %poke   mark  result-type  ^-  result
          :*  path.req  dap.bowl  aeon.req
              ?-  what.req
                %wave  wave/(got:wav wav.tide aeon.req)
                %rock  ?:  =(aeon.req 0)  rock/*rock:lake
                       rock/(got:rok rok.tide aeon.req)
      ==  ==  ==
    ::
        %pine
      =/  =aeon
        ?-  what.req
          %rock  key:(fall (pry:rok rok.tide) *[=key val]:rok)
          %wave  key:(fall (ram:wav wav.tide) *[=key val]:wav)
        ==
      :*  %pass   (zoom response/pine/(scot %p src.bowl)^from.req^what.req^path.req)
          %agent  [src.bowl from.req]
          %poke   mark  result-type  ^-  result
          [path.req dap.bowl aeon what.req]
      ==
    ==
  --
--
