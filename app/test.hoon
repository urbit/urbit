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
++  peek  _~
++  report-error
  |=  [a=spur b=(each cage tang)]  ^-  tang
  =/  should-fail  (~(get by failing) (flop a))
  ?-    -.b
      %&
    ?~  should-fail  ~
    :~  leaf+"warn: expected failure, {<`tape`u.should-fail>}"
        leaf+"warn: built succesfully"
        (sell q.p.b)
    ==
  ::
      %|
    ?^  should-fail
      ~[>[%failed-known `tape`(weld "TODO: " u.should-fail)]<]
    (flop p.b)
  ==
::
++  made-a-core
  |=  {a/spur @uvH b/gage}
  :_  +>.$
  ?>  ?=([%tabl [(each) (each)] ~] b)
  %-  (slog (report-error a p.i.p.b))
  =/  nex/(list spur)
    =<(p ;;(,[%& %cont * p=(list spur)] q.i.p.b))
  ?~  nex  ~&(%cores-tested ~)
  [ost (build-core nex)]~
::
++  build-core
  |=  [a=spur b=(list spur)]
  ~&  >>  (flop a)
  :^  %exec  a-core+a  our
  %-  some
  ^-  bilk
  :-  now-beak
  :~  %tabl
    [[%core now-beak a] [%$ %cont !>(b)]]
  ==
::
++  made-a-rend
  |=  {a/wire @uvH b/gage}
  ?>  ?=([ren=mark ~] a)
  =+  `[ren=term pax=path]`?~(a !! a)
  :_  +>.$
  ?>  ?=([%tabl [(each) (each)] ~] b)
  %-  (slog (report-error /[ren.i.a]/ren p.i.p.b))
  =/  nex/(list term)
    =<(p ;;(,[%& %cont * p=(list term)] q.i.p.b))
  ?~  nex  ~&(%rens-tested ~)
  [ost (build-rend nex)]~
::
++  build-rend
  |=  [a=term b=(list term)]
  ~&  >>  [%ren a]
  =/  bem/beam  (need (de-beam %/example))
  =.  -.bem  now-beak
  :^  %exec  a-rend+/[a]  our
  %-  some
  ^-  bilk
  :-  -.bem
  :~  %tabl
    [`silk`[%bake a fake-fcgi bem] [%$ %cont !>(b)]]
  ==
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
++  skip-completely
  ^~  ^-  (map path tape)
  %-  my  :~ ::TODO don't hardcode
    :-  /ren/css            "not meant to be called outside /web/pack"
    :-  /ren/js             "not meant to be called outside /web/pack"
    :-  /ren/run            "not meant to be called except on a (different) hoon file"
  ::
    :-  /app/gh             "hangs for some reason"
    :-  /mar/gh             "hangs for some reason"
    :-  /app/twit           "slow and/or crash"
    :-  /gen/twit           "slow and/or crash"
    :-  /mar/twit           "slow and/or crash"
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
