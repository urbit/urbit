::
::::  /mar/talk/action/hoon
  ::
/-    talk
/+    talk-json
::
|_  act/action:talk
::
++  grab                                                ::>  convert from
  |%
  ++  noun  action:talk                                 ::<  from %noun
  ++  json                                              ::>  from %json
    =>  [. dejs:talk-json]  ::TODO  =,
    =,  dejs-soft:format
    |=  a/json
    ^-  action:talk
    =-  (need ((of -) a))
    :~  create+(ot nom+so des+so sec+secu ~)
        delete+(ot nom+so why+(mu so) ~)
        depict+(ot nom+so des+so ~)
        filter+(ot nom+so fit+filt ~)
        permit+(ot nom+so inv+bo sis+(as (su fed:ag)) ~)
        source+(ot nom+so aub+bo srs+(as sorc) ~)
        ::
        convey+(ar thot)
        phrase+(ot aud+audi ses+(ar spec:dejs:talk-json) ~)
        ::
        notify+(ot aud+audi pes+pres ~)
        naming+(ot aud+audi man+huma ~)
        ::
        glyph+(ot gyf+so aud+audi bin+bo ~)
        nick+(ot who+(su fed:ag) nic+so ~)
    ==
  --
::
++  grow                                                ::>  convert to
  |%
  ++  json                                              ::>  to %json
    =>  [. enjs:talk-json]  ::TODO  =,
    =,  enjs:format
    %+  frond  -.act
    ::>  only %convey has just a single piece of data.
    ?:  ?=($convey -.act)  a+(turn tos.act thot)
    %-  pairs
    ?-  -.act
      $create  ~[nom+s+nom.act des+s+des.act sec+s+sec.act]
      $delete  ~[nom+s+nom.act why+(mabe why.act cord:enjs)]
      $depict  ~[nom+s+nom.act des+s+des.act]
      $filter  ~[nom+s+nom.act fit+(filt fit.act)]
      $permit  ~[nom+s+nom.act inv+b+inv.act sis+(sa sis.act ship)]
      $source  ~[nom+s+nom.act sub+b+sub.act srs+(sa srs.act sorc)]
      ::
      $phrase  ~[aud+(audi aud.act) ses+a+(turn ses.act spec:enjs)]
      ::
      $notify  ~[aud+(audi aud.act) pes+s+pes.act]
      $naming  ~[aud+(audi aud.act) man+(huma man.act)]
      ::
      $glyph   ~[gyf+s+gyf.act aud+(sa aud.act circ) bin+b+bin.act]
      $nick    ~[who+(ship who.act) nic+s+nic.act]
    ==
  --
--
