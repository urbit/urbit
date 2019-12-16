/+  default-agent
::
|%
+$  card  card:agent:gall
+$  test
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
++  build-core
  |=  [=disc:ford a=spur b=(list spur)]
  ^-  card
  ~&  >>  (flop a)
  :*  %pass  a-core+a
      %arvo  %f  %build
      live=|
      ^-  schematic:ford
      :-  [%core disc %hoon a]
      [%$ %cont !>(b)]
  ==
--
::
=,  ford
=,  format
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init   on-init:def
++  on-save   on-save:def
++  on-load   on-load:def
++  on-poke
  |=  [=mark =vase]
  :_  this
  |^
  =+  !<(a=test vase)
  ?-    -.a
      %arvo     ~|(%stub !!) ::basically double solid?
      %hoons    ~&((list-hoons p.a ~) ~)
      %names    ~&((list-names p.a) ~)
      %marks    ~|(%stub !!) ::TODO restore historical handler
      %renders   ~&(%all-renderers-are-disabled ~)
      %cores
    =/  spurs  [- +]:(list-hoons p.a skip=(sy /sys /ren /tests ~))
    [(build-core [p q]:byk.bowl spurs) ~]
  ==
  ::
  ++  now-beak  %_(byk.bowl r [%da now.bowl])
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
  ++  list-names
    |=  a/path  ^-  (list term)
    =/  hon  (list-hoons a ~)
    %+  turn  hon
    |=  b=spur
    (join '-' (slag 1 (flop b)))
  ::
  ++  skip-completely
    ^~  ^-  (map path tape)
    %-  my  :~ ::TODO don't hardcode
      :-  /ren/run            "not meant to be called except on a (different) hoon file"
      :-  /ren/test-gen       "temporarily disabled"
    ==
  --
::
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo
  |=  [=wire =sign-arvo]
  |^
  :_  this
  ^-  (list card)
  ?.  ?=([%a-core *] wire)
    (on-arvo:def wire sign-arvo)
  ?.  ?=(%made +<.sign-arvo)
    (on-arvo:def wire sign-arvo)
  =/  =spur  t.wire
  =/  res  result.sign-arvo
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
    ;;  [%success %$ %cont * p=(list ^spur)]
    tail.build-result.res
  ?~  nex  ~&(%cores-tested ~)
  [(build-core [p q]:byk.bowl nex) ~]
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
::
++  on-fail   on-fail:def
--
