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
    ::=,  de-json:talk-json
    =,  dejs-soft:format
    |=  a/json
    ^-  action:talk
    =-  (need ((of -) a))
    :~  create+(ot nom+so des+so sec+secu:de-json:talk-json ~)
        delete+(ot nom+so why+(mu so) ~)
        depict+(ot nom+so des+so ~)
        filter+(ot nom+so fit+filt:de-json:talk-json ~)
        permit+(ot nom+so inv+bo sis+(as:de-json:talk-json (su fed:ag)) ~)
        source+(ot nom+so aub+bo srs+(as:de-json:talk-json sorc:de-json:talk-json) ~)
        ::
        convey+(ar thot:de-json:talk-json)
        phrase+(ot aud+audi:de-json:talk-json ses+(ar spec:de-json:talk-json) ~)
        ::
        notify+(ot aud+audi pes+pres ~):de-json:talk-json
        naming+(ot aud+audi man+huma ~):de-json:talk-json
        ::
        glyph+(ot gyf+so aud+audi:de-json:talk-json bin+bo ~)
        nick+(ot who+(su fed:ag) nic+so ~)
    ==
  --
::
++  grow                                                ::>  convert to
  |%
  ++  json                                              ::>  to %json
    ::=,  en-json:talk-json
    =,  enjs:format
    %+  frond  -.act
    ::>  only %convey has just a single piece of data.
    ?:  ?=($convey -.act)  a+(turn tos.act thot:en-json:talk-json)
    %-  pairs
    ?-  -.act
      $create  ~[nom+s+nom.act des+s+des.act sec+s+sec.act]
      $delete  ~[nom+s+nom.act why+(mabe:en-json:talk-json why.act cord:en-json:talk-json)]
      $depict  ~[nom+s+nom.act des+s+des.act]
      $filter  ~[nom+s+nom.act fit+(filt:en-json:talk-json fit.act)]
      $permit  ~[nom+s+nom.act inv+b+inv.act sis+(sa:en-json:talk-json sis.act ship)]
      $source  ~[nom+s+nom.act sub+b+sub.act srs+(sa:en-json:talk-json srs.act sorc:en-json:talk-json)]
      ::
      $phrase  ~[aud+(audi:en-json:talk-json aud.act) ses+a+(turn ses.act spec:en-json:talk-json)]
      ::
      $notify  ~[aud+(audi:en-json:talk-json aud.act) pes+s+pes.act]
      $naming  ~[aud+(audi:en-json:talk-json aud.act) man+(huma:en-json:talk-json man.act)]
      ::
      $glyph   ~[gyf+s+gyf.act aud+(sa:en-json:talk-json aud.act circ:en-json:talk-json) bin+b+bin.act]
      $nick    ~[who+(ship who.act) nic+s+nic.act]
    ==
  --
--
