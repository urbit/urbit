::  verb-json: conversions for verb events
::
/-  verb
::
|%
++  enjs
  =,  enjs:format
  |%
  ++  event
    |=  event-plus:verb
    %-  pairs
    :~  'act'^(numb act)
        'now'^(time now)  ::  ms timestamp, lossy-ness is fine here
        'src'^s+(scot %p src)
        'sap'^s+(spat sap)
        'kind'^s+-.cause
        'deets'^(^cause cause)
        'effects'^a+(turn effects effect)
    ==
  ::
  ++  cause
    |=  =cause:verb
    ^-  json
    ?-  -.cause
      %on-init   b+&
      %on-load   b+&
      %on-poke   (pairs 'mark'^s+mark.cause 'mug'^(mug mug.cause) ~)
      %on-watch  (path path.cause)
      %on-leave  (path path.cause)
      %on-agent  %-  pairs
                 :~  'wire'^(path wire.cause)
                     'sign'^s+-.sign.cause
                   ::
                     :-  'deets'
                     ?-  -.sign.cause
                       %poke-ack   b+ack.sign.cause
                       %watch-ack  b+ack.sign.cause
                       %kick       ~
                       %fact       %-  pairs
                                   :~  'mark'^s+mark.sign.cause
                                       'mug'^(mug mug.sign.cause)
                                   ==
                     ==
                 ==
      %on-arvo   %-  pairs
                 :~  'wire'^(path wire.cause)
                     'vane'^s+vane.cause
                     'sign'^s+sign.cause
                 ==
      %on-fail   s+term.cause
    ==
  ::
  ++  effect
    |=  effect:verb
    ^-  json
    %-  pairs
    :-  'kind'^s++<-
    :_  ~
    :-  'deets'
    %-  pairs
    ^-  (list [@t json])
    ?-  +<-
      %poke   :~  'wire'^(path wire)
                  'gill'^(^gill gill)
                  'mark'^s+mark
                  'mug'^(^mug mug)
              ==
      %watch  ~['wire'^(^path wire) 'gill'^(^gill gill) 'path'^(^path path)]
      %leave  ~['wire'^(path wire) 'gill'^(^gill gill)]
      %fact   ~['paths'^a+(turn paths path) 'mark'^s+mark 'mug'^(^mug mug)]
      %kick   ~['paths'^a+(turn paths path)]
      %arvo   ~['wire'^(path wire) 'vane'^s+vane 'task'^s+task]
    ==
  ::
  ++  gill  |=(=gill:gall `json`s+(rap 3 (scot %p p.gill) '/' q.gill ~))
  ++  mug   |=(mug=@ux `json`s+(crip ((x-co:co 8) mug)))
  --
--