::  interactively view and approve permissions for a desk
::
/-  *sole
/+  *generators, perms
|%
::  $permissions: state of permissions for a desk
::
+$  permissions
  $:  blocking-commit=pers:gall
      blocking-live=pers:gall
      approved=pers:gall
      requested=pers:gall
      in-seal=pers:gall
  ==
::  $output: final result given to %hood
::
+$  output  [%helm-pass note-arvo]
::  ++get-permissions: scry out seal file and $dome then construct $permissions
::
++  get-permissions
  |=  [our=@p now=@da dek=desk]
  ^-  permissions
  =+  .^(=dome:clay %cx (en-beam [our %$ da+now] /domes/(scot %tas dek)))
  =/  in-seal=pers:gall
    ?.  .^(? %cu (en-beam [our dek da+now] /desk/seal))
      *pers:gall
    %-  ~(gas in *pers:gall)
    +:.^([%0 (list perm:gall)] %cx (en-beam [our dek da+now] /desk/seal))
  ::
  :*  ?~(cop.dome *pers:gall mis.u.cop.dome)
      lac.dome
      pes.dome
      ask.dome
      in-seal
  ==
--
::
:-  %ask
|=  [[now=@da * bek=beak] [dek=desk ~] ~]
::
?.  (~(has in .^((set desk) %cd (en-beam [p.bek %$ da+now] /))) dek)
  (print leaf+"Error: {<dek>} not found" no-product)
=/  all-perms=permissions  (get-permissions p.bek now dek)
=/  live=(map dude:gall desk)  (scry-live:perms p.bek now)
=/  apps=(map desk @t)
  %-  ~(gas by *(map desk @t))
  %+  turn  ~(tap in .^((set desk) %cd (en-beam [p.bek %$ da+now] /)))
  |=(=desk [desk *@t])
|^  ^-  (sole-product output)
(summary all-perms)
::  +summary: print a summary of desk permissions and prompt with choices
::
++  summary
  |=  permissions
  |^  ^-  (sole-product output)
  =/  =tang
    :~  ''
        leaf+"Blocking commit:  {(a-co:co ~(wyt in blocking-commit))}"
        leaf+"Blocking live:    {(a-co:co ~(wyt in blocking-live))}"
        ''
    ::
        %^  approved-fraction  "Total:            "  approved
        (~(uni in in-seal) (~(uni in requested) approved))
    ::
        %^  approved-fraction  "Additional:       "  approved
        (~(uni in requested) (~(dif in approved) in-seal))
    ::
        (approved-fraction "Seal file:        " approved in-seal)
        ''
        leaf+"Permission summary for {<dek>}:"
        ''
    ==
  (summary-prompt tang & |)
  ::  +summary-prompt: print options and prompt after summary
  ::
  ++  summary-prompt
    |=  [=tang options=? error=?]
    =?  tang  error    ['error: invalid choice' tang]
    =?  tang  options
      :*  '---------------------------------------------------------'
          '[S]eal, [A]pproved, [R]equested, Blocking [L]ive/[C]ommit'
          '[Q]uit or view:'
          '---------------------------------------------------------'
          tang
      ==
    :^  tang
    %|  [& %$ ""]
    |=  in=tape
    ?:  |(=(['Q' ~] in) =(['q' ~] in))
      (produce %helm-pass %d %hail ~)
    ?:  |(=(['S' ~] in) =(['s' ~] in))
      pp-abet:(pp-passport &):(pp-abed:pp in-seal)
    ?:  |(=(['A' ~] in) =(['a' ~] in))
      pp-abet:(pp-passport &):(pp-abed:pp approved)
    ?:  |(=(['R' ~] in) =(['r' ~] in))
      pp-abet:(pp-passport &):(pp-abed:pp requested)
    ?:  |(=(['L' ~] in) =(['l' ~] in))
      pp-abet:(pp-passport &):(pp-abed:pp blocking-live)
    ?:  |(=(['C' ~] in) =(['c' ~] in))
      pp-abet:(pp-passport &):(pp-abed:pp blocking-commit)
    (summary-prompt ~ | &)
  --
::  +approved-fraction: print fraction of permissions approved
::
++  approved-fraction
  |=  [name=tape approved=pers:gall =pers:gall]
  ^-  tank
  :-  %leaf
  "{name}".
  "{(a-co:co ~(wyt in (~(int in pers) approved)))}".
  "/{(a-co:co ~(wyt in pers))} approved"
::  +pp: passport/permissions pretty-printing core
::
++  pp
  |_  $:  =passport:perms
          =pers:gall
          out=(sole-product output)
      ==
  ++  pp-core    .
  ::  +pp-abet: produce final result
  ::
  ++  pp-abet    out
  ::  +pp-abed: initialize +pp core
  ::
  ++  pp-abed
    |=  =pers:gall
    %=  pp-core
      passport     ((perm-tree:perms live apps *pers:gall) pers)
      pers         pers
      out          [~ %& ~ %helm-pass %d %hail ~]
    ==
  ::  +pp-indent: create an indent, maybe with bullet point
  ::
  ++  pp-indent
    |=  [lvl=@ud bullet=?]
    %+  weld
      (reap (mul 4 lvl) ' ')
    ?.(bullet "" "- ")
  ::  +pp-passport: print a passport
  ::
  ++  pp-passport
    |=  simple=?
    ^+  pp-core
    =.  p.out    ['' p.out]
    =.  pp-core  (pp-rad 0 simple)
    =.  pp-core  (pp-sys 0 simple)
    =.  pp-core  (pp-any 0 simple)
    =.  pp-core  (pp-new 0 simple)
    =.  pp-core  (pp-app 0 simple)
    (pp-prompt & |)
  ::  +pp-prompt: options & prompt after printing something
  ::
  ++  pp-prompt
    |=  [options=? error=?]
    ^+  pp-core
    =?  p.out  error    ['Error: invalid choice' p.out]
    =?  p.out  options
      :*  '----------------------------------'
          '[S]imple, [D]etailed, [R]aw'
          '[Q]uit, [B]ack, [A]pprove or view:'
          '----------------------------------'
          p.out
      ==
    %=    pp-core
        out
      :^  p.out  %|  [& %$ ""]
      |=  in=tape
      ?:  |(=(['Q' ~] in) =(['q' ~] in))
        =.  out  [~ %& ~ %helm-pass %d %hail ~]
        pp-abet
      ?:  |(=(['B' ~] in) =(['b' ~] in))
        (summary all-perms)
      ?:  |(=(['A' ~] in) =(['a' ~] in))
        =/  =note-arvo
          [%c %curb dek (~(uni ^in approved.all-perms) pers)]
        =.  out  [~ %& ~ %helm-pass note-arvo]
        pp-abet
      ?:  |(=(['D' ~] in) =(['d' ~] in))
        pp-abet:(pp-passport |)
      ?:  |(=(['S' ~] in) =(['s' ~] in))
        pp-abet:(pp-passport &)
      ?:  |(=(['R' ~] in) =(['r' ~] in))
        pp-abet:pp-raw
      pp-abet:(pp-prompt | &)
    ==
  ::  +pp-many: print a list of either perm categories or descriptions
  ::
  ++  pp-many
    |=  [dent=@ud simple=? many=perm-many:perms]
    ^+  pp-core
    %+  roll  many
    |:  [once=*perm-once:perms pp-core]
    ?-    -.once
        %node  (pp-node:pp-core dent +.once)
        %kind
      ?:  simple
        (pp-kind:pp-core dent once)
      %+  roll  ~(tap in pes.once)
      |:  [node=*perm-node:perms pp-core]
      (pp-node dent node)
    ==
  ::  +pp-rad: print dangerous permissions and warnings
  ::
  ++  pp-rad
    |=  [dent=@ud simple=?]
    ^+  pp-core
    =.  p.out
      ['' [%rose ["" (pp-indent dent |) ""] 'Dangerous permissions:' ~] p.out]
    ?.  .?(rad.passport)
      pp-core(p.out ['' [%rose ["" (pp-indent +(dent) |) ""] 'none' ~] p.out])
    =.  pp-core  (pp-many +(dent) simple rad.passport)
    =.  p.out  ['' p.out]
    (pp-warnings +(dent) rad.passport)
  ::  +pp-sys: print kernel permissions
  ::
  ++  pp-sys
    |=  [dent=@ud simple=?]
    ^+  pp-core
    =.  p.out
      ['' [%rose ["" (pp-indent dent |) ""] 'System permissions:' ~] p.out]
    ?.  .?(sys.passport)
      pp-core(p.out ['' [%rose ["" (pp-indent +(dent) |) ""] 'none' ~] p.out])
    =.  pp-core  (pp-many +(dent) simple sys.passport)
    pp-core(p.out ['' p.out])
  ::  +pp-any: print permissions that apply to all apps
  ::
  ++  pp-any
    |=  [dent=@ud simple=?]
    ^+  pp-core
    =.  p.out
      ['' [%rose ["" (pp-indent dent |) ""] 'All apps permissions:' ~] p.out]
    ?.  .?(any.passport)
      pp-core(p.out ['' [%rose ["" (pp-indent +(dent) |) ""] 'none' ~] p.out])
    =.  pp-core  (pp-many +(dent) simple any.passport)
    pp-core(p.out ['' p.out])
  ::  +pp-new: print unknown app permissions
  ::
  ++  pp-new
    |=  [dent=@ud simple=?]
    ^+  pp-core
    =.  p.out
      ['' [%rose ["" (pp-indent dent |) ""] 'Unknown app permissions:' ~] p.out]
    ?.  .?(new.passport)
      pp-core(p.out ['' [%rose ["" (pp-indent +(dent) |) ""] 'none' ~] p.out])
    =.  pp-core  (pp-many +(dent) simple new.passport)
    pp-core(p.out ['' p.out])
  ::  +pp-app: print per-app permissions
  ::
  ++  pp-app
    |=  [dent=@ud simple=?]
    ^+  pp-core
    =.  p.out
      ['' [%rose ["" (pp-indent dent |) ""] 'Specific app permissions:' ~] p.out]
    ?.  .?(app.passport)
      pp-core(p.out ['' [%rose ["" (pp-indent +(dent) |) ""] 'none' ~] p.out])
    %+  roll  app.passport
    |:  [*[app=@t pes=perm-many:perms] pp-core]
    =.  p.out
      :_  p.out
      [%rose ["" (pp-indent +(dent) |) ""] (cat 3 app ':') ~]
    (pp-many +(+(dent)) simple pes)
  ::  +pp-node: print an individual permission description
  ::
  ++  pp-node
    |=  [dent=@ud node=perm-node:perms]
    ^+  pp-core
    =/  =tank
      [%rose ["" (pp-indent dent &) ""] desc.node ~]
    pp-core(p.out [tank p.out])
  ::  +pp-kind: print individual permission category with node count
  ::
  ++  pp-kind
    |=  [dent=@ud kind=$>(%kind perm-once:perms)]
    ^+  pp-core
    =/  node-count=@ud  ~(wyt in pes.kind)
    =/  =tank
      :+  %rose
        ["" (pp-indent dent &) ""]
      :~  %^  cat  3
            nom.kind
          ?:  (lte node-count 1)
            ''
          (crip " ({(a-co:co node-count)})")
      ==
    pp-core(p.out [tank p.out])
  ::  +pp-warnings: print warnings
  ::
  ++  pp-warnings
    |=  [dent=@ud rad=perm-many:perms]
    ^+  pp-core
    =?  p.out  ?=(^ rad)
      %-  weld
      :_  p.out
      ^-  tang
      :~  ''
        :+  %rose
          ["" (pp-indent dent |) ""]
        :~  'WARNING! this may allow the app to:'
        ==
      ==
    =;  warnings=tang
      ?~  warnings  pp-core
      pp-core(p.out ['' (weld warnings p.out)])
    %~  tap  in
    %+  roll  rad
    |=  [once=perm-once:perms warnings=(set tank)]
    ?-    -.once
        %node
      ?~  warn.once  warnings
      %-  ~(put in warnings)
      `tank`[%rose ["" (pp-indent +(dent) &) ""] u.warn.once ~]
    ::
        %kind
      %-  ~(gas in warnings)
      %+  murn  ~(tap in pes.once)
      |=  node=perm-node:perms
      ^-  (unit tank)
      ?~  warn.node  ~
      (some [%rose ["" (pp-indent +(dent) &) ""] u.warn.node ~])
    ==
  ::  +pp-raw: print raw permissions
  ::
  ++  pp-raw
    ^+  pp-core
    =.  p.out
      %-  weld
      ^-  [tang tang]
      :_  ['' p.out]
      :-  ''
      %+  turn
        (sort ~(tap in pers) |=([a=* b=*] (aor b a)))
      |=(=perm:gall `tank`leaf+<perm>)
    (pp-prompt & |)
  --
--
