::
::::  /hoon/talk/lib
  ::
  ::  This file is in the public domain.
  ::
/?    310
/-    talk
!:
::::
  ::
[. ^talk]
|_  bol/bowl
++  main                                                ::  main story
  |=  who/ship  ^-  cord
  =+  can=(clan who)
  ?+  can  %porch
    $czar  %court
    $king  %floor
  ==
::
::TODO  add to zuse?
++  true-self
  |=  who/ship
  ?.  ?=($earl (clan who))  who
  (sein who)
::
++  said-url                                            ::  app url
  |=  url/purl
  :^  ost.bol  %poke  /said-url
  :+  [our.bol %talk]  %talk-action
  ^-  action
  :+  %phrase
    [[%& our.bol (main our.bol)] ~ ~]
  [%app dap.bol (crip (earn url))]~   :: XX
::
++  said                                                ::  app message
  |=  {our/@p dap/term now/@da eny/@uvJ mes/(list tank)}
  :-  %talk-action
  ^-  action
  :-  %convey
  |-  ^-  (list thought)
  ?~  mes  ~
  :_  $(mes t.mes, eny (sham eny mes))
  ^-  thought
  :+  (shaf %thot eny)
    [[[%& our (main our)] [*envelope %pending]] ~ ~]
  [now *bouquet [%app dap (crip ~(ram re i.mes))]]
::
++  uniq
  |=  eny/@uvJ
  ^-  (quid serial eny)
  [(shaf %serial eny) (shax eny)]
::
++  change-glyphs                                       ::<  ...
  ::>
  ::
  |=  {gys/(jug char (set partner)) bin/? gyf/char pas/(set partner)}
  ^+  gys
  ::  simple bind.
  ?:  bin  (~(put ju gys) gyf pas)
  ::  unbind all of glyph.
  ?~  pas  (~(del by gys) gyf)
  ::  unbind single.
  (~(del ju gys) gyf pas)
::
++  change-nicks                                        ::<  ...
  ::>
  ::
  |=  {nis/(map ship cord) who/ship nic/cord}
  ^+  nis
  ?:  =(nic '')
    (~(del by nis) who)
  (~(put by nis) who nic)
::
++  change-friends                                      ::<  ...
  ::>
  ::
  |=  {cis/(set circle) add/? cir/circle}
  ^+  cis
  %.  cir
  ?:  add
    ~(put in cis)
  ~(del in cis)
::
++  change-config                                       ::<  ...
  ::>
  ::
  |=  {cof/config dif/diff-config}
  ^+  cof
  ?-  -.dif
    $full     cof.dif
    $caption  cof(cap cap.dif)
    $filter   cof(fit fit.dif)
    $remove   cof
    ::
      $sourcee
    %=  cof
        sre
      %.  `(set partner)`pas.dif  ::TODO?  why do we *need* to cast?
      ~&  [%doing-sourcee add.dif pas.dif]
      ?:  add.dif
        ~(uni in sre.cof)
      ~(dif in sre.cof)
    ==
    ::
      $permit
    %=  cof
        ses.con
      %.  sis.dif
      ?:  add.dif
        ~(uni in ses.con.cof)
      ~(dif in ses.con.cof)
    ==
    ::
      $secure
    %=  cof
        sec.con
      sec.dif
      ::
        ses.con
      ?.  .=  ?=(?($white $green) sec.dif)
              ?=(?($white $green) sec.con.cof)
        ~
      ses.con.cof
    ==
  ==
::
++  change-status                                       ::<  ...
  ::>
  ::
  |=  {sat/status dif/diff-status}
  ^+  sat
  ?-  -.dif
    $full       sat.dif
    $presence   sat(pec pec.dif)
    $remove     sat
    ::
      $human
    %=  sat
        man
      ?-  -.dif.dif
        $full     man.dif.dif
        $true     [tru.dif.dif han.man.sat]
        $handle   [tru.man.sat han.dif.dif]
      ==
    ==
  ==
::
++  depa                                              ::  de-pathing core
  =>  |%  ++  grub  *                                 ::  result
          ++  weir  (list coin)                       ::  parsed wire
          ++  fist  $-(weir grub)                     ::  reparser instance
      --
  |%
  ::
  ++  al
    |*  {hed/$-(coin *) tal/fist}
    |=  wir/weir  ^+  [*hed *tal]
    ?~  wir  !!
    [(hed i.wir) (tal t.wir)]
  ::
  ++  at
    |*  typ/{@tas (pole @tas)}
    =+  [i-typ t-typ]=typ
    |=  wer/weir
    ^-  (tup:dray i-typ t-typ)  ::< ie, (tup %p %tas ~) is {@p @tas}
    ?~  wer  !!
    ?~  t-typ
      ?^  t.wer  !!
      ((do i-typ) i.wer)
    :-  ((do i-typ) i.wer)
    (^$(typ t-typ) t.wer)
  ::
  ++  mu                                              ::  true unit
    |*  wit/fist
    |=  wer/weir
    ?~(wer ~ (some (wit wer)))
  ::
  ++  af                                              ::  object as frond
    |*  buk/(pole {cord fist})
    |=  wer/weir
    ?>  ?=({{$$ $tas @tas} *} wer)
    ?~  buk  !!
    =+  [[tag wit] t-buk]=buk
    ?:  =(tag q.p.i.wer)
      [tag ~|(tag+`@tas`tag (wit t.wer))]
    ?~  t-buk  ~|(bad-tag+q.p.i.wer !!)
    (^$(buk t-buk) wer)
  ::
  ++  or
    |*  typ/|-($@(@tas {@tas $}))
    |=  con/coin
    ::^-  _(snag *@ (turn (limo typ) |*(a/@tas [a (odo:raid a)])))
    ?>  ?=($$ -.con)
    =/  i-typ  ?@(typ typ -.typ)
    ?:  =(i-typ p.p.con)
      :-  i-typ
      ^-  (odo:raid i-typ)
      q.p.con
    ?@  typ  ~|(%bad-odor !!)
    (^$(typ +.typ) con)
  ::
  ++  do
    |*  typ/@tas
    |=  con/coin
    ^-  (odo:raid typ)
    ?.  ?=($$ -.con)  ~|(%not-dime !!)
    ?.  =(typ p.p.con)  ~|(bad-odor+`@tas`p.p.con !!)
    q.p.con
  ::
  ++  ul                                              ::  null
    |=(wer/weir ?~(wer ~ !!))
  ::
  ++  un
    |*  wit/$-(coin *)
    |=  wir/weir  ^+  *wit
    ?~  wir  !!
    ?^  t.wir  !!
    (wit i.wir)
  --
--
