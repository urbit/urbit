::
|%
++  test
  $%  [%arvo ~]       ::UNIMPLEMENTED 
      [%marks ~]      ::UNIMPLEMENTED  
      [%cores p=path]
      [%hoons p=path]
      [%names p=path]
      [%renders p=path]
  ==
--
::
|%
++  join
  |=  {a/cord b/(list cord)}
  ?~  b  ''
  (rap 3 |-([i.b ?~(t.b ~ [a $(b t.b)])]))
::
++  fake-fcgi  [%many [%blob *cred:eyre] $+[%n ~] ~]
--
::
=,  gall
=,  ford
=,  format
|_  {bowl $~}
::
++  peek  _~
::
++  report-error
  |=  [=spur bud=build-result]
  ^-  tang
  =/  should-fail  (~(get by failing) (flop spur))
  ?-    -.bud
      %success
    ?~  should-fail  ~
    :~  leaf+"warn: expected failure, {<`tape`u.should-fail>}"
        leaf+"warn: built succesfully"
        ?:  ?=(%bake +<.bud)
          (sell q.cage.bud)
        ?>  ?=(%core +<.bud)
        (sell vase.bud)
    ==
  ::
      %error
    ?^  should-fail
      ~[>[%failed-known `tape`(weld "TODO: " u.should-fail)]<]
    (flop message.bud)
  ==
::
++  made-a-core
  |=  [=spur @da res=made-result]
  :_  +>.$
  ?:  ?=([%incomplete *] res)
    ~&  incomplete-core+spur
    ((slog tang.res) ~)
  ?.  ?=([%complete %success *] res)
    ~&  unsuccessful-core+spur
    ((slog message.build-result.res) ~)
  ?>  ?=(^ +<.build-result.res)
  %-  (slog (report-error spur head.build-result.res))
  =/  nex=(list ^spur)
    =<  p
    ;;  ,[%success %$ %cont * p=(list ^spur)]
    tail.build-result.res
  ?~  nex  ~&(%cores-tested ~)
  [ost (build-core nex)]~
::
++  build-core
  |=  [a=spur b=(list spur)]
  ~&  >>  (flop a)
  :-  %build
  :^    a-core+a
      our
    live=|
  ^-  schematic:ford
  :-  [%core now-disc %hoon a]
  [%$ %cont !>(b)]
::
++  made-a-rend
  |=  [=spur @da res=made-result]
  :_  +>.$
  ?>  ?=([ren=term ~] spur)
  =+  `[ren=term pax=path]`?~(spur !! spur)
  ?:  ?=([%incomplete *] res)
    ~&  incomplete-core+spur
    ((slog tang.res) ~)
  ?.  ?=([%complete %success *] res)
    ~&  unsuccessful-core+spur
    ((slog message.build-result.res) ~)
  ?>  ?=(^ +<.build-result.res)
  %-  (slog (report-error /[ren]/ren head.build-result.res))
  =/  nex=(list term)
    =<  p
    ;;  ,[%success %$ %cont * p=(list term)]
    tail.build-result.res
  ?~  nex  ~&(%rens-tested ~)
  [ost (build-rend nex)]~
::
++  build-rend
  |=  [a=term b=(list term)]
  ~&  >>  [%ren a]
  :-  %build
  :^    a-rend+/[a]
      our
    live=|
  ^-  schematic:ford
  :: XX what should the rail be?
  :-  [%bake a fake-fcgi now-disc /example]
  [%$ %cont !>(b)]
::
++  poke-noun
  |=  a=test
  :_  +>
  ?-    -.a
      %arvo  ~|(%stub !!) ::basically double solid?
      %hoons  ~&((list-hoons p.a ~) ~)
      %cores  [ost (build-core [- +]:(list-hoons p.a skip=(sy /sys /ren ~)))]~
      %names  ~&((list-names p.a) ~)
      %marks  ~|(%stub !!) ::TODO restore historical handler
      %renders  :: XX temporarily disabled
                :: [ost (build-rend [- +]:(list-names (weld /ren p.a)))]~
                ~
  ==    
::
++  list-names
  |=  a/path  ^-  (list term)
  =/  hon  (list-hoons a ~)
  %+  turn  hon
  |=  b=spur
  (join '-' (slag 1 (flop b)))
::
++  list-hoons
  |=  [under=path skipping=(set spur)]  ^-  (list spur)
  =/  sup  (flop under)
  ~&  [%findining-hoons under=under]
  |-  ^-  (list spur)
  %-  zing
  %+  turn
    =-  (sort ~(tap by -) aor)
    dir:.^(arch %cy (en-beam now-beak sup))
  |=  [a=knot ~]  ^-  (list spur)
  =.  sup  [a sup]
  ?:  (~(has in skipping) (flop sup))
    ~&(> [(flop sup) %out-of-scope] ~)
  =/  ded  (~(get by skip-completely) (flop sup))
  ?^  ded
    ~&(> [(flop sup) %skipped `tape`u.ded] ~)
  ?~  [fil:.^(arch %cy (en-beam now-beak [%hoon sup]))]
    ^$
  ~&  (flop sup)
  [sup ^$]
::
++  now-beak  %_(byk r [%da now])
++  now-disc  `disc:ford`[p.byk q.byk]
++  skip-completely
  ^~  ^-  (map path tape)
  %-  my  :~ ::TODO don't hardcode
    :-  /ren/css            "not meant to be called outside /web/pack"
    :-  /ren/js             "not meant to be called outside /web/pack"
    :-  /ren/run            "not meant to be called except on a (different) hoon file"
    :-  /ren/collections    "temporarily disabled"
    :-  /ren/x-urb          "temporarily disabled"
    :-  /ren/x-htm          "temporarily disabled"
    :-  /ren/x-collections-snip          "temporarily disabled"
    :-  /ren/x-collections-json          "temporarily disabled"
    :-  /ren/urb            "temporarily disabled"
  ::
    :-  /app/gh             "hangs for some reason"
    :-  /mar/gh             "hangs for some reason"
    :-  /app/twit           "slow and/or crash"
    :-  /gen/twit           "slow and/or crash"
    :-  /mar/twit           "slow and/or crash"
    :-  /web/landscape           "/$ doensn't work in tests"
  ==
::
++  failing
  ^~  ^-  (map path tape)
  %-  my  :~ ::TODO don't hardcode
  ::
    :-  /app/pipe           "wants 'flavor:hall' to exist"
    :-  /gen/capitalize     "wants unicode-data/txt"
  ::
    :-  /lib/down-jet/parse       "// nonsense"
    :-  /lib/down-jet/rend        "// nonsense"
    :-  /lib/hood/kiln            "ford can't handle surs from libs"
    :-  /lib/sole                 "ford can't handle surs from libs"
    :-  /lib/hall                 "ford can't handle surs from libs"
    :-  /lib/twitter              "ford can't handle surs from libs"
    :-  /sys/arvo                 "BROKEN"
    :-  /sys/vane/jael            "expects our"
    :-  /sys/vane/xmas            "expects our"
  ==
--
