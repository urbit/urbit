::
::::  /lib/hall/hoon
  ::
/-    hall
::
::::
  ::
[. ^hall]
|_  bol/bowl:gall
::
::TODO  add to zuse?
++  true-self
  |=  who/ship
  ?.  ?=($earl (clan:title who))  who
  (sein:title who)
::
++  above
  |=  who/ship
  ?:  ?=($czar (clan:title who))  ~zod
  (sein:title who)
::
++  said-url                                            ::  app url
  |=  url/purl:eyre
  :^  ost.bol  %poke  /said-url
  :+  [our.bol %hall]  %hall-action
  ^-  action
  :+  %phrase
    [[our.bol %inbox] ~ ~]
  [%app dap.bol %lin | (crip (en-purl:html url))]~   :: XX
::
++  said                                                ::  app message
  |=  mes/(list tank)
  :-  %hall-action
  ^-  action
  :-  %phrase
  :-  [[our.bol %inbox] ~ ~]
  |-  ^-  (list speech)
  ?~  mes  ~
  :_  $(mes t.mes)
  ^-  speech
  [%app dap.bol %lin | (crip ~(ram re i.mes))]
::
++  uniq
  ^-  {serial _eny.bol}
  [(shaf %serial eny.bol) (shax eny.bol)]
::
::TODO  add to zuse?
++  simple-wrap
  |=  {txt/tape wyd/@ud}
  ^-  (list tape)
  ?~  txt  ~
  =+  ^-  {end/@ud nex/?}
    ?:  (lte (lent txt) wyd)  [(lent txt) &]
    =+  ace=(find " " (flop (scag +(wyd) `tape`txt)))
    ?~  ace  [wyd |]
    [(sub wyd u.ace) &]
  :-  (tufa (scag end `(list @)`txt))
  $(txt (slag ?:(nex +(end) end) `tape`txt))
::
++  range-to-path
  :>    msg range to path
  :>
  :>  turns a range structure into a path used for
  :>  subscriptions.
  ::
  |=  ran/range
  ^-  path
  ?~  ran  ~
  %+  welp
    /(scot -.hed.u.ran +.hed.u.ran)
  ?~  tal.u.ran  ~
  /(scot -.u.tal.u.ran +.u.tal.u.ran)
::
++  path-to-range
  :>    path to msg range
  :>
  :>  turns the tail of a subscription path into a
  :>  range structure, skipping over non-range terms.
  ::
  |=  pax/path
  ^-  range
  ?~  pax  ~
  ::  skip past non-number parts of path.
  ?:  ?=({$~ $~} [(slaw %da i.pax) (slaw %ud i.pax)])
    $(pax t.pax)
  :+  ~
    =+  hed=(slaw %da i.pax)
    ?^  hed  [%da u.hed]
    [%ud (slav %ud i.pax)]
  ?~  t.pax  ~
  :-  ~
  =+  tal=(slaw %da i.t.pax)
  ?^  tal  [%da u.tal]
  [%ud (slav %ud i.t.pax)]
::
++  change-glyphs                                       :<  ...
  ::
  |=  {gys/(jug char audience) bin/? gyf/char aud/audience}
  ^+  gys
  ::  simple bind.
  ?:  bin  (~(put ju gys) gyf aud)
  ::  unbind all of glyph.
  ?~  aud  (~(del by gys) gyf)
  ::  unbind single.
  (~(del ju gys) gyf aud)
::
++  change-nicks
  :>    change nick map
  :>
  :>  changes a nickname in a map, adding if it doesn't
  :>  yet exist, removing if the nickname is empty.
  ::
  |=  {nis/(map ship cord) who/ship nic/cord}
  ^+  nis
  ?:  =(nic '')
    (~(del by nis) who)
  (~(put by nis) who nic)
::
++  change-config
  :>  applies a config diff to the given config.
  ::
  |=  {cof/config dif/diff-config}
  ^+  cof
  ?-  -.dif
    $full     cof.dif
    $caption  cof(cap cap.dif)
    $filter   cof(fit fit.dif)
    $remove   cof
  ::
      $usage
    %=  cof
        tag
      %.  tas.dif
      ?:  add.dif
        ~(uni in tag.cof)
      ~(dif in tag.cof)
    ==
  ::
      $source
    %=  cof
        src
      %.  src.dif
      ?:  add.dif
        ~(put in src.cof)
      ~(del in src.cof)
    ==
  ::
      $permit
    %=  cof
        sis.con
      %.  sis.dif
      ?:  add.dif
        ~(uni in sis.con.cof)
      ~(dif in sis.con.cof)
    ==
  ::
      $secure
    %=  cof
        sec.con
      sec.dif
    ::
        sis.con
      ?.  .=  ?=(?($white $green) sec.dif)
              ?=(?($white $green) sec.con.cof)
        ~
      sis.con.cof
    ==
  ==
::
++  change-status
  :>  applies a status diff to the given status.
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
        $true     [han.man.sat tru.dif.dif]
        $handle   [han.dif.dif tru.man.sat]
      ==
    ==
  ==
::
::TODO  annotate all!
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
    ^-  (tup:dray:wired i-typ t-typ)  ::  ie, (tup %p %tas ~) is {@p @tas}
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
    ?~  t-buk  ~|(bad-tag+`@tas`q.p.i.wer !!)
    (^$(buk t-buk) wer)
  ::
  ++  or
    =+  tmp=|-($@(@tas {@tas $}))  ::TODO typ/that syntax-errors...
    |*  typ/tmp
    |=  con/coin
    ::^-  _(snag *@ (turn (limo typ) |*(a/@tas [a (odo:raid:wired a)])))
    ?>  ?=($$ -.con)
    =/  i-typ  ?@(typ typ -.typ)
    ?:  =(i-typ p.p.con)
      :-  i-typ
      ^-  (odo:raid:wired i-typ)
      q.p.con
    ?@  typ  ~|(%bad-odor !!)
    (^$(typ +.typ) con)
  ::
  ++  do
    |*  typ/@tas
    =/  typecheck  `@tas`typ
    |=  con/coin
    ^-  (odo:raid:wired typ)
    ?.  ?=($$ -.con)  ~|(%not-dime !!)
    ?.  =(typ p.p.con)  ~|(bad-odor+`@tas`p.p.con !!)
    q.p.con
  ::
  ++  ul                                              ::  null
    |=(wer/weir ?~(wer ~ !!))
  ::
  ++  un
    |*  wit/$-(coin *)
    |=  wer/weir  ^+  *wit
    ?~  wer  !!
    ?^  t.wer  !!
    (wit i.wer)
  --
--
