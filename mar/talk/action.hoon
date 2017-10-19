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
    =,  de-json:talk-json
    =,  dejs-soft:format
    |=  a/json
    ^-  action:talk
    =-  (need ((of -) a))
    :~  create+(ot nom+so des+so sec+pres ~)
        delete+(ot nom+so why+(mu so) ~)
        depict+(ot nom+so des+so ~)
        filter+(ot nom+so fit+filt ~)
        permit+(ot nom+so inv+bo sis+audi ~)
        source+(ot nom+so aub+bo srs+(as sorc) ~)
        ::
        convey+(ar thot)
        phrase+(ot aud+audi ses+(ar spec) ~)
        ::
        notify+(ot aud+audi pes+prec ~)
        naming+(ot aud+audi man+huma ~)
        ::
        glyph+(ot gyf+so aud+audi bin+bo ~)
        nick+(ot who+ship nic+so ~)
    ==
  --
::
++  grow                                                ::>  convert to
  |%
  ++  json                                              ::>  to %json
    =,  en-json:talk-json
    =,  enjs:format
    %+  frond  -.act
    ::>  only %convey has just a single piece of data.
    ?:  ?=($convey -.act)  a+(turn tos.act thot)
    %-  pairs
    ?-  -.act
      $create  ~[nom+s+nom.act des+s+des.act sec+s+sec.act]
      $delete  ~[nom+s+nom.act why+?~(why.sec ~ s+why.sec)]
      $depict  ~[nom+s+nom.act des+s+des.act]
      $filter  ~[nom+s+nom.act fit+(filt fit.act)]
      $permit  ~[nom+s+nom.act inv+b+inv.act sis+(sa sis.act ship)]
      $source  ~[nom+s+nom.act sub+b+sub.act srs+(sa srs.act sorc)]
      ::
      $phrase  ~[aud+(sa aud.act circ) ses+a+(turn ses.act spec)]
      ::
      $notify  ~[aud+(sa cis.act circ) pes+(prec pes.act)]
      $naming  ~[aud+(sa cis.act circ) man+(huma man.act)]
      ::
      $glyph   ~[gyf+s+gyf.act aud+(sa circ) bin+b+bin.act]
      $nick    ~[who+s+(ship who.act) nic+s+nic.act]
    ==
  --
--
