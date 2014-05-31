::
:: /=main=/autobegin/hoon
::
=>  .(-< `who=@p`-<)
=>  .(+ =>(+ ^/===/lib/pony))
|=  [est=time sen=@uw]
|=  [cod=@t ~]
^-  bowl
=+  jos=(rush cod apex:poja)
=<  (boss jos)
|%
++  ot                                                  ::  object type
  |=  o=(unit jval)  ^-  (map ,@t jval)
  =+  p=(need o)
  ?+(-.p !! %o p.p)
++  st                                                  ::  string type
  |=  v=(unit jval)  ^-  @t
  =+  w=(need v)
  ?+(-.w !! %s p.w)
++  su                                                  ::  string unit
  |=  v=(unit jval)  ^-  (unit ,@t)
  ?~  v
    ~
  =+  r=(st v)
  ?~  r
    ~
  `r
++  pi                                                  ::  phonetic integer
  |=  v=(unit jval)  ^-  @p
  =+  t=(fix (st v))
  (need (slaw %p t))
++  fix                                                 ::  correct format
  |=  t=@t  ^-  @t
  ?.  =('~' (end 3 1 t))
    (rap 3 ~['~' t])
  t
++  boss
  |=  son=(unit jval)  ^-  bowl
  =+  lav=(mule |.((dook son)))
  ?-  -.lav
      %.y  p.lav
      %.n  ~[~[[%ho p.lav] [%xy /d `card`[%logo 111]]]]
  ==
++  dook
  |=  son=(unit jval)  ^-  bowl
  =+  m=(ot son)
  =+  teg=~(get by m)
  =+  bud=~|(%bad-ship (pi (teg %ship)))
  =+  tic=~|(%bad-ticket (pi (teg %ticket)))
  =+  eny=~|(%bad-entropy (st (teg %entr)))
  =+  ges=(form bud m)
  =+  bos=(sein bud)
  ?>  !=(bos bud)
  ?>  !=(who bud)
  (moss est bud tic (shax eny) ges)
++  form
  |=  [bud=@p m=(map ,@t jval)]  ^-  gens
  ~|  %bad-form
  =+  can=`@tas`(clan bud)
  ?>  =(can %duke)                                      :: XX support others
  =+  teg=~(get by m)
  =+  poe=(st (teg %pose))
  =+  lag=(gins (st (teg %lang)))
  =>  .(poe (?(%lord %lady %punk %anon ~) poe))
  ?-  poe
      ~
    !!
      %anon
    `gens`[lag (gcos can `what`[%anon ~])]
      ?(%lord %lady)
    `gens`[lag (gcos can (what poe (mane m)))]
      %punk
    `gens`[lag (gcos can (mana m))]
  ==
++  gins
  |=  l=@ta  ^-  lang
  ~|  %bad-lang
  ?~((glon l) !! l)
++  mana
  |=  m=(map ,@t jval)  ^-  what
  ~|  %bad-name
  =+  teg=~(get by m)
  =+  ban=((hard sect) (st (teg %bann)))
  =+  nam=(st (teg %hand))
  [%punk p=ban q=nam]
++  mane
  |=  m=(map ,@t jval)  ^-  whom
  ~|  %bad-name
  =+  teg=~(get by m)
  =+  fir=(st (teg %fnam))
  =+  mid=(su (teg %mnam))
  =+  nik=(su (teg %nnam))
  =+  las=(st (teg %lnam))
  =+  ban=((hard sect) (st (teg %bann)))
  =+  yer=(need (rush (st (teg %dobr)) dem))
  =+  gov=`path`(need (rush (st (teg %loca)) moon))
  (whom yer gov ban `name`[fir mid nik las])
++  moon
  ;~  pose
    ;~  plug  (cook |=([a=@ b=@] (cat 3 a b)) ;~(plug low low))
      ;~  pfix  fas
        %+  cook
          |=(a=tape (rap 3 ^-((list ,@) a)))
        (star ;~(pose hig hep nud))
      ==
      (easy ~)
    ==
  ==
++  moss
  |=  [now=@da bud=@p tic=@p tey=@ ges=gens]
  ^-  bowl
  =+  bur=(shax (mix (jam ges) tey))
  =+  loy=(bruw 2.048 bur)
  %-  (post (sein bud) %ta [bud tic ges pub:ex:loy])
  |=  [now=@da rup=(unit ,*)]
  :_  ~
  ?~  rup  ~[[la/leaf/"request rejected"] [%xy /d `card`[%logo 111]]]
  =+  mac=`mace`[[0 sec:ex:loy] ~]
  =+  wil=((hard (unit will)) u.rup)
  ?~  wil
    :~  [%la %leaf "request rejected - invalid ticket"]
        [%xy /d `card`[%logo 111]]
    ==
  :~  [%la %leaf "request approved"]
      [%xy /a `card`[%cash bud mac u.wil]]
  ==
--
