/+  default-agent
|%
+$  opus-id  [=mark =ship app=term =path]
+$  opera    (map opus-id (unit vase))
+$  subber
  $_  ^|
  |_  [=bowl:gall =opera]
  ++  on-init
    *[(list card) (set opus-id) _^|(..on-init)]
  ::
  ++  on-save
    *vase
  ::
  ++  on-load
    |~  vase
    *[(list card) (set opus-id) _^|(..on-init)]
  ::
  ++  on-poke
    |~  [mark vase]
    *[(list card) (set opus-id) _^|(..on-init)]
  ::
  ++  on-watch
    |~  path
    *[(list card) (set opus-id) _^|(..on-init)]
  ::
  ++  on-leave
    |~  path
    *[(list card) (set opus-id) _^|(..on-init)]
  ::
  ++  on-peek
    |~  path
    *(unit (unit cage))
  ::
  ++  on-agent
    |~  [wire sign:agent:gall]
    *[(list card) (set opus-id) _^|(..on-init)]
  ::
  ++  on-arvo
    |~  [wire sign-arvo]
    *[(list card) (set opus-id) _^|(..on-init)]
  ::
  ++  on-fail
    |~  [term tang]
    *[(list card) (set opus-id) _^|(..on-init)]
  --
++  default
  |*  [agent=* help=*]
  |_  [=bowl:gall =opera]
  +*  def  ~(. (default-agent agent help) bowl)
  ++  on-init   =+(r=on-init:def [-.r ~(key by opera) +.r])
  ++  on-save   on-save:def
  ++  on-load   |=(=vase =+(r=(on-load:def +<) [-.r ~(key by opera) +.r]))
  ++  on-poke   |=(=cage =+(r=(on-poke:def +<) [-.r ~(key by opera) +.r]))
  ++  on-watch  |=(=path =+(r=(on-watch:def +<) [-.r ~(key by opera) +.r]))
  ++  on-leave  |=(=path =+(r=(on-leave:def +<) [-.r ~(key by opera) +.r]))
  ++  on-peek   on-peek:def
  ++  on-agent  |=([wire sign:agent:gall] =+(r=(on-agent:def +<) [-.r ~(key by opera) +.r]))
  ++  on-arvo   |=([=wire =sign-arvo] =+(r=(on-arvo:def +<) [-.r ~(key by opera) +.r]))
  ++  on-fail   |=([=term =tang] =+(r=(on-fail:def +<) [-.r ~(key by opera) +.r]))
  --
::
+$  card  card:agent:gall
+$  state-0
  $:  %0
      =opera
      inner-state=vase
  ==
::
++  run
  |=  =subber
  =|  =opera
  =>  |%
      ++  process-subs
        |=  subs=(set opus-id)
        ^-  (quip card ^opera)
        =/  new  ~(tap in (~(dif in subs) ~(key by opera)))
        =/  del  ~(tap in (~(dif in ~(key by opera)) subs))
        =|  cards=(list card)
        |-  ^-  (quip card ^opera)
        ?^  new
          =.  opera  (~(put by opera) i.new ~)
          =.  cards
            :_  cards
            [%pass (opus-wire i.new) [%agent [ship app] %watch path]:i.new]
          $(new t.new)
        ?^  del
          =.  opera  (~(del by opera) i.del)
          =.  cards
            :_  cards
            [%pass (opus-wire i.del) [%agent [ship app] %leave ~]:i.del]
          $(del t.del)
        [(flop cards) opera]
      ::
      ++  opus-wire
        |=  opus-id
        (weld /opus/[mark]/(scot %p ship)/[app] path)
      ::
      ++  de-opus
        |=  =wire
        ^-  opus-id
        ?>  ?=([%opus @ @ @ *] wire)
        :*  mark=i.t.wire
            ship=(slav %p i.t.t.wire)
            app=i.t.t.t.wire
            path=t.t.t.t.wire
        ==
      --
  ^-  agent:gall
  |_  =bowl:gall
  +*  this  .
      og   ~(. subber bowl opera)
      def  ~(. (default-agent this %|) bowl)
  ++  on-init
    ^-  [(list card) agent:gall]
    =/  [cards-1=(list card) subs=(set opus-id) core=^subber]
      on-init:og
    =.  subber  core
    =^  cards-2  opera  (process-subs subs)
    [(weld cards-1 cards-2) this]
  ++  on-load
    |=  =old=vase
    ^-  [(list card) agent:gall]
    =+  !<(=state-0 old-vase)
    =.  opera  opera.state-0
    =/  [cards-1=(list card) subs=(set opus-id) core=^subber]
      (on-load:og inner-state.state-0)
    =.  subber  core
    =^  cards-2  opera  (process-subs subs)
    [(weld cards-1 cards-2) this]
  ++  on-save
    ^-  vase
    !>(`state-0`[%0 opera on-save:og])
  ++  on-poke
    |=  [=mark =vase]
    ^-  [(list card) agent:gall]
    =/  [cards-1=(list card) subs=(set opus-id) core=^subber]
      (on-poke:og mark vase)
    =.  subber  core
    =^  cards-2  opera  (process-subs subs)
    [(weld cards-1 cards-2) this]
  ::
  ++  on-watch
    |=  =path
    ^-  [(list card) agent:gall]
    =/  [cards-1=(list card) subs=(set opus-id) core=^subber]
      (on-watch:og path)
    =.  subber  core
    =^  cards-2  opera  (process-subs subs)
    [(weld cards-1 cards-2) this]
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  [(list card) agent:gall]
    =.  opera
      ?.  ?=([%opus @ @ @ *] wire)
        opera
      =/  =opus-id  (de-opus wire)
      ?+   -.sign  opera
          %kick  (~(del by opera) opus-id)
          %watch-ack
        ?~  p.sign
          opera
        (~(del by opera) opus-id)
      ::
          %fact
        ?:  =(mark.opus-id p.cage.sign)
          (~(put by opera) opus-id `q.cage.sign)
        =+  .^  =dais:clay  %cb
                /(scot %p our.bowl)/home/(scot %da now.bowl)/[mark.opus-id]
                ==
        ?.  =(form:dais p.cage.sign)
          %-  (slog >[%opus-mark diff=form:dais got=p.cage.sign opus-id]< ~)
          opera
        =/  opus  (~(get by opera) opus-id)
        ?~  opus
          %-  (slog >[%strange-opus opus-id]< ~)
          opera
        ?~  u.opus
          %-  (slog >[%opus-diff-no-full opus-id]< ~)
          opera
        (~(put by opera) opus-id `(~(pact dais u.u.opus) q.cage.sign))
      ==
    ::
    =/  [cards-1=(list card) subs=(set opus-id) core=^subber]
      (on-agent:og wire sign)
    =.  subber  core
    =^  cards-2  opera  (process-subs subs)
    [(weld cards-1 cards-2) this]
  ::
  ++  on-leave
    |=  =path
    ^-  [(list card) agent:gall]
    =/  [cards-1=(list card) subs=(set opus-id) core=^subber]
      (on-leave:og path)
    =.  subber  core
    =^  cards-2  opera  (process-subs subs)
    [(weld cards-1 cards-2) this]
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  [(list card) agent:gall]
    =/  [cards-1=(list card) subs=(set opus-id) core=^subber]
      (on-arvo:og wire sign-arvo)
    =.  subber  core
    =^  cards-2  opera  (process-subs subs)
    [(weld cards-1 cards-2) this]
  ++  on-fail
    |=  [=term =tang]
    ^-  [(list card) agent:gall]
    =/  [cards-1=(list card) subs=(set opus-id) core=^subber]
      (on-fail:og term tang)
    =.  subber  core
    =^  cards-2  opera  (process-subs subs)
    [(weld cards-1 cards-2) this]
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    (on-peek:og path)
  --
--
