::
::::  /mar/hall/action/hoon
  ::
/-    hall
/+    hall-json
::
|_  act/action:hall
::
++  grab                                                ::>  convert from
  |%
  ++  noun  action:hall                                 ::<  from %noun
  ++  json                                              ::>  from %json
    =>  [. dejs:hall-json]  ::TODO  =,
    =,  dejs-soft:format
    |=  a/json
    ^-  action:hall
    =-  (need ((of -) a))
    :~  create+(ot nom+so des+so sec+secu ~)
        design+(ot nom+so cof+conf ~)
        delete+(ot nom+so why+(mu so) ~)
        depict+(ot nom+so des+so ~)
        filter+(ot nom+so fit+filt ~)
        permit+(ot nom+so inv+bo sis+(as (su fed:ag)) ~)
        source+(ot nom+so sub+bo srs+(as sorc) ~)
        usage+(ot nom+so add+bo tas+(as so) ~)
        ::
        convey+(ar thot)
        phrase+(ot aud+audi ses+(ar spec:dejs:hall-json) ~)
        ::
        notify+(ot aud+audi pes+(mu pres) ~)
        naming+(ot aud+audi man+huma ~)
        ::
        glyph+(ot gyf+so aud+audi bin+bo ~)
        nick+(ot who+(su fed:ag) nic+so ~)
        ::
        public+(ot add+bo cir+circ ~)
    ==
  --
::
++  grow                                                ::>  convert to
  |%
  ++  json                                              ::>  to %json
    =>  [. enjs:hall-json]  ::TODO  =,
    =,  enjs:format
    %+  frond  -.act
    ::>  only %convey has just a single piece of data.
    ?:  ?=($convey -.act)  a+(turn tos.act thot)
    %-  pairs
    ?-  -.act
      $create  ~[nom+s+nom.act des+s+des.act sec+s+sec.act]
      $design  ~[nom+s+nom.act cof+(conf cof.act)]
      $delete  ~[nom+s+nom.act why+(mabe why.act cord:enjs)]
      $depict  ~[nom+s+nom.act des+s+des.act]
      $filter  ~[nom+s+nom.act fit+(filt fit.act)]
      $permit  ~[nom+s+nom.act inv+b+inv.act sis+(sa sis.act ship)]
      $source  ~[nom+s+nom.act sub+b+sub.act srs+(sa srs.act sorc)]
      $usage   ~[nom+s+nom.act add+b+add.act tas+(sa tas.act cord:enjs)]
      ::
      $phrase  ~[aud+(audi aud.act) ses+a+(turn ses.act spec:enjs)]
      ::
      $notify  ~[aud+(audi aud.act) pes+(mabe pes.act cord:enjs)]
      $naming  ~[aud+(audi aud.act) man+(huma man.act)]
      ::
      $glyph   ~[gyf+s+gyf.act aud+(sa aud.act circ) bin+b+bin.act]
      $nick    ~[who+(ship who.act) nic+s+nic.act]
      ::
      $public  ~[add+b+add.act cir+(circ cir.act)]
    ==
  --
--
