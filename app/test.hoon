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
  :+  a-core+a
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
  :+  a-rend+/[a]
    live=|
  ^-  schematic:ford
  =/  bem=beam  (need (de-beam %/example))
  =/  =rail  [[p q] s]:bem
  :-  [%bake a fake-fcgi rail]
  [%$ %cont !>(b)]
::
++  poke-noun
  |=  a=test
  :_  +>
  ?-    -.a
      %arvo  ~|(%stub !!) ::basically double solid?
      %hoons  ~&((list-hoons p.a ~) ~)
      %cores  [ost (build-core [- +]:(list-hoons p.a skip=(sy /sys /ren /tests ~)))]~
      %names  ~&((list-names p.a) ~)
      %marks  ~|(%stub !!) ::TODO restore historical handler
      %renders  [ost (build-rend [- +]:(list-names (weld /ren p.a)))]~
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
    :-  /ren/test-gen       "temporarily disabled"
    :-  /ren/urb            "temporarily disabled"
    :-  /ren/x-urb          "temporarily disabled"
    :-  /ren/x-htm          "temporarily disabled"
    :-  /ren/x-collections-snip          "temporarily disabled"
    :-  /ren/x-collections-json          "temporarily disabled"
  ::
    :-  /web/landscape      "/$ doensn't work in tests"
  ==
::
++  failing
  ^~  ^-  (map path tape)
  %-  my  :~ ::TODO don't hardcode
  ::
    :-  /gen/al                "compiler types out-of-date"
    :-  /gen/musk              "compiler types out-of-date"
  ::
    :-  /gen/cosmetic          "incomplete"
    :-  /gen/lust              "incomplete"
    :-  /gen/scantastic        "incomplete"
  ==
--
