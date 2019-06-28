!:
:-  %say
|=  *
:-  %noun
=-  %hello
=>  |%
    ++  tope                                            ::  topographic type
      $@  $?  %&                                        ::  cell or atom
              %|                                        ::  atom
          ==                                            ::
      (pair tope tope)                                  ::  cell
    --
|%
++  ax
  =+  :*  dom=`axis`1
          doc=*(list what)
      ==
  |_  mod/tile
  ++  home
    ::  express a hoon against the original subject
    ::
    |=(gen/hoon ^-(hoon ?:(=(1 dom) gen [%tsgr [%$ dom] gen])))
  ::
  ++  default
    ::  produce a hoon that makes the model's default value, untyped
    ::
    |-  ^-  hoon
    ?-    mod
        {^ *}
      [$(mod -.mod) $(mod +.mod)]
    ::
        {$axil *}
      ?+  p.mod  [%rock %$ 0]
        $cell  [[%rock %$ 0] [%rock %$ 0]]
        $void  [%zpzp ~]
      ==
    ::
        {$bark *}
      $(mod q.mod)
    ::
        {$herb *}
      =+  cys=~(boil ap p.mod)
      ?:  ?=($herb -.cys)
        (home [%tsgl [%limb %$] p.mod])
      $(mod cys)
    ::
        {$deet *}
      $(mod q.mod)
    ::
        {$fern *}
      ::  last entry is the default value
      ::
      |-  ^-  hoon
      ?~(t.p.mod ^$(mod i.p.mod) $(i.p.mod i.t.p.mod, t.p.mod t.t.p.mod))
    ::
        {$kelp *}
      ::  last entry is the default value
      ::
      |-  ^-  hoon
      ?~(t.p.mod ^$(mod i.p.mod) $(i.p.mod i.t.p.mod, t.p.mod t.t.p.mod))
    ::
        {$leaf *}
      [%rock p.mod q.mod]
    ::
        {$plow *}
      $(mod q.mod)
    ::
        {$reed *}
      $(mod p.mod)
    ::
        {$vine *}
      $(mod q.mod)
    ::
        {$weed *}
      (home p.mod)
    ==
  ::
  ++  trivial
    ::  ersatz by trivial construction
    ::
    ^-  hoon
    :+  %tsls
      [%bust %noun]
    ~(construct sample [2 %|])
  ::
  ++  basic
    |=  bas/base
    ?-    bas
    ::
        {$atom *}
      ::  trivial zero
      ::
      [%sand p.bas 0]
    ::
        $noun
      ::  raw nock produces noun type
      ::
      =+([%rock %$ 0] [%ktls [%dttr - - [%rock %$ 1]] -])
    ::
        $cell
      ::  reduce to pair of nouns
      ::
      =+($(mod [%axil %noun]) [- -])
    ::
        $bean
      ::  comparison produces boolean type
      ::
      =+([%rock %$ 0] [%ktls [%dtts - -] -])
    ::
        $null
      [%rock %n 0]
    ::
        $void
      ::  should not actually be a thing
      ::
      [%zpzp ~]
    ==
  ::
  ++  decorate
    ::  document
    ::
    |=  gen/hoon
    ^-  hoon
    ?~  doc  gen
    =/  fin  $(doc t.doc)
    ?~(i.doc gen [%docs u.i.doc gen])
  ::
  ++  ersatz
    ::  produce a correctly typed instance without subject
    ::
    ^-  hoon
    ?-    mod
        {^ *}
      %-  decorate
      =.  doc  ~
      [ersatz(mod -.mod) ersatz(mod +.mod)]
    ::
        {$axil *}
      (decorate (basic p.mod))
    ::
        {$bark *}
      [%ktts p.mod ersatz(mod q.mod)]
    ::
        {$herb *}
      %-  decorate
      =.  doc  ~
      =+  cys=~(boil ap p.mod)
      ?:  ?=($herb -.cys)
        (home [%tsgl [%limb %$] p.mod])
      ersatz(mod cys)
    ::
        {$deet *}
      [%dbug p.mod ersatz(mod q.mod)]
    ::
        {$fern *}
      trivial
    ::
        {$kelp *}
      trivial
    ::
        {$leaf *}
      (decorate [%rock p.mod q.mod])
    ::
        {$plow *}
      ersatz(mod q.mod, doc [p.mod doc])
    ::  atom/cell, $@
    ::
        {$reed *}
      trivial
    ::  pair/switch, $^
    ::
        {$vine *}
      trivial
    ::
        {$weed *}
      (home p.mod)
    ==
  ::
  ++  factory
    ::  produce a normalizing gate (mold)
    ::
    ^-  hoon
    :^  %brts  ~^~
      [%base %noun]
    ~(construct sample [6 %&])
  ::
  ++  sample
    ::  normalize a sample of the subject
    ::
    |_  $:  ::  axe: axis to sample
            ::  top: topographic type of sample
            ::
            axe/axis
            top/tope
        ==
    ++  basic
      |=  bas/base
      ::  apply documentation
      ::
      ?^  doc  document
      ?-    bas
          {%atom *}
        ::  rez: fake instance
        ::
        =/  rez  ersatz
        ?^  top  rez
        ?:  =(%| top)
          ::  xx sanitize
          ::
          fetch
        [%wtpt fetch-wing fetch rez]
      ::
          $noun
        fetch
      ::
          $cell
        ?^  top  fetch
        ::  rez: fake instance
        ::
        =/  rez  ersatz
        ?:  =(%| top)
          rez
        [%wtpt fetch-wing rez fetch]
      ::
          $bean
        ?^  top  ersatz
        :^    %wtcl
            [%dtts [%rock %$ |] [%$ axe]]
          [%rock %f |]
        [%rock %f &]
      ::
          $null
        ersatz
      ::
          $void
        ersatz
      ==
    ++  fetch
      ::  load the sample
      ::
      ^-  hoon
      [%$ axe]
    ::
    ++  fetch-wing
      ::  load, as a wing
      ::
      ^-  wing
      [[%& axe] ~]
    ::
    ++  choice
      ::  match full models, by trying them
      ::
      |=  $:  ::  one: first option
              ::  rep: other options
              ::
              one/tile
              rep/(list tile)
          ==
      ^-  hoon
      ::  if no other choices, construct head
      ::
      ?~  rep  construct(mod one)
      ::  fin: loop completion
      ::
      =/  fin/hoon  $(one i.rep, rep t.rep)
      ::  new: trial product
      ::  old: original subject
      ::
      =/  new  [%$ 2]
      =*  old  [%$ 3]
      ::  build trial noun
      ::
      :+  %tsls
        ::  build the sample with the first option
        ::
        construct(mod one)
      ::  build test
      ::
      :^    %wtcl
          ::  if the trial noun equals the sample
          ::
          [%dtts new fetch]
        ::  produce the trial noun
        ::
        new
      ::  continue with the original subject
      ::
      [%tsgr old fin]
    ::
    ++  switch
      |=  $:  ::  one: first format
              ::  two: more formats
              ::
              one/line
              rep/(list line)
          ==
      ^-  hoon
      ::  if no other choices, construct head
      ::
      ?~  rep  construct(mod `tile`one)
      ::  fin: loop completion
      ::
      =/  fin/hoon  $(one i.rep, rep t.rep)
      ::  interrogate this instance
      ::
      :^    %wtcl
          ::  test if we match this wing
          ::
          [%wtts p.i.rep fetch-wing]
        ::  use this format
        ::
        :-  `hoon`p.i.rep
        construct(mod q.i.rep, top &, axe (peg axe 3))
      ::  continue in the loop
      ::
      fin
    ::
    ++  probe
      ::  probe for cell or default
      ::
      ^-  hoon
      ::  against constructor
      ::
      :+  %tsgr
        ::  constructor trap
        ::
        :+  %brdt  ~^~
        ::  construct within trap
        ::
        %=  construct
        ::  old context within trap context
        ::
          dom  (peg 3 dom)
        ::  old sample within trap sample
        ::
          axe  (peg 3 axe)
        ::  only kick trap if sample is known cell
        ::
          top  [& &]
        ==
      ::  boc: call constructor
      ::  but: default, but coerce type to call
      ::
      =/  boc/hoon  [%limb %$]
      =/  but/hoon  [%ktls boc default]
      ?:  =(& top)
        ::  may be atom or cell; default or construct
        ::
        [%wtpt fetch-wing but boc]
      ::  must be atom; construct
      ::
      but
    ::
    ++  document
      ::  document and construct
      ::
      |-  ^-  hoon
      ?~  doc  construct
      =/  fin  $(doc t.doc)
      ?~(i.doc fin [%docs u.i.doc fin])
    ::
    ++  construct
      ::  constructor at arbitrary sample
      ::
      ^-  hoon
      ?-    mod
      ::
      ::  cell
      ::
          {^ *}
        ::  apply help
        ::
        ?^  doc  document
        ::  probe unless we know the sample is a cell
        ::
        ?@  top  probe
        ::  if known cell, descend directly
        ::
        :-  construct(mod -.mod, top p.top, axe (peg axe 2))
        construct(mod +.mod, top q.top, axe (peg axe 3))
      ::
      ::  base
      ::
          {$axil *}
        (basic p.mod)
      ::
      ::  name, $=
      ::
          {$bark *}
        [%ktts p.mod construct(mod q.mod)]
      ::
      ::  debug
      ::
          {$deet *}
        [%dbug p.mod construct(mod q.mod)]
      ::
      ::  choice, $?
      ::
          {$fern *}
        (choice i.p.mod t.p.mod)
      ::
      ::  synthesis, $;
      ::
          {$herb *}
        ?^  doc  document
        =+  cys=~(boil ap p.mod)
        ?:  ?=($herb -.cys)
          [%cnhp (home p.mod) fetch ~]
        construct(mod cys)
      ::
      ::  switch, $%
      ::
          {$kelp *}
        ::  if atom or unknown, probe
        ::
        ?@  top  probe
        ::  if cell, enter switch directly
        ::
        (switch i.p.mod t.p.mod)
      ::
      ::  constant
      ::
          {$leaf *}
        (decorate [%rock p.mod q.mod])
      ::
      ::  documentation
      ::
          {$plow *}
        construct(doc [p.mod doc], mod q.mod)
      ::
      ::  branch, $@
      ::
          {$reed *}
        ?^  doc  document
        ?@  top
          ?:  =(%| top)
            construct(mod p.mod)
          [%wtpt fetch-wing construct(mod p.mod) construct(mod q.mod)]
        construct(mod q.mod)
      ::
      ::  bridge, $^
      ::
          {$vine *}
        ?^  doc  document
        ?@  top  probe
        :^    %wtpt
            fetch-wing(axe (peg axe 2))
          construct(mod q.mod)
        construct(mod p.mod)
      ::
      ::  weed, $_
      ::
          {$weed *}
        (decorate (home p.mod))
      ==
    --
  --
--
