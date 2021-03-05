::  shoe: console application library
::
::    /lib/sole: draw some characters
::    /lib/shoe: draw the rest of the fscking app
::
::    call +agent with a type, then call the resulting function with a core
::    of the shape described in +shoe.
::    you may produce classic gall cards and "shoe-effects", shorthands for
::    sending cli events to connected clients.
::    default implementations for the shoe-specific arms are in +default.
::    for a simple usage example, see /app/shoe.
::
/-  *sole
/+  sole, auto=language-server-complete
|%
+$  state-0
  $:  %0
      soles=(map @ta sole-share)
  ==
::  $card: standard gall cards plus shoe effects
::
+$  card
  $%  card:agent:gall
      [%shoe sole-ids=(list @ta) effect=shoe-effect]  ::  ~ sends to all soles
  ==
::  $shoe-effect: easier sole-effects
::
+$  shoe-effect
  $%  ::  %sole: raw sole-effect
      ::
      [%sole effect=sole-effect]
      ::  %table: sortable, filterable data, with suggested column char widths
      ::
      [%table head=(list dime) wide=(list @ud) rows=(list (list dime))]
      ::  %row: line sections with suggested char widths
      ::
      [%row wide=(list @ud) cols=(list dime)]
  ==
::  +shoe: gall agent core with extra arms
::
++  shoe
  |*  command-type=mold
  $_  ^|
  |_  bowl:gall
  ::  +command-parser: input parser for a specific session
  ::
  ::    if the head of the result is true, instantly run the command
  ::
  ++  command-parser
    |~  sole-id=@ta
    |~(nail *(like [? command-type]))
  ::  +tab-list: autocomplete options for the session (to match +command-parser)
  ::
  ++  tab-list
    |~  sole-id=@ta
    ::  (list [@t tank])
    *(list (option:auto tank))
  ::  +on-command: called when a valid command is run
  ::
  ++  on-command
    |~  [sole-id=@ta command=command-type]
    *(quip card _^|(..on-init))
  ::
  ++  can-connect
    |~  sole-id=@ta
    *?
  ::
  ++  on-connect
    |~  sole-id=@ta
    *(quip card _^|(..on-init))
  ::
  ++  on-disconnect
    |~  sole-id=@ta
    *(quip card _^|(..on-init))
  ::
  ::NOTE  standard gall agent arms below, though they may produce %shoe cards
  ::
  ++  on-init
    *(quip card _^|(..on-init))
  ::
  ++  on-save
    *vase
  ::
  ++  on-load
    |~  vase
    *(quip card _^|(..on-init))
  ::
  ++  on-poke
    |~  [mark vase]
    *(quip card _^|(..on-init))
  ::
  ++  on-watch
    |~  path
    *(quip card _^|(..on-init))
  ::
  ++  on-leave
    |~  path
    *(quip card _^|(..on-init))
  ::
  ++  on-peek
    |~  path
    *(unit (unit cage))
  ::
  ++  on-agent
    |~  [wire sign:agent:gall]
    *(quip card _^|(..on-init))
  ::
  ++  on-arvo
    |~  [wire sign-arvo]
    *(quip card _^|(..on-init))
  ::
  ++  on-fail
    |~  [term tang]
    *(quip card _^|(..on-init))
  --
::  +default: bare-minimum implementations of shoe arms
::
++  default
  |*  [shoe=* command-type=mold]
  |_  =bowl:gall
  ++  command-parser
    |=  sole-id=@ta
    (easy *[? command-type])
  ::
  ++  tab-list
    |=  sole-id=@ta
    ~
  ::
  ++  on-command
    |=  [sole-id=@ta command=command-type]
    [~ shoe]
  ::
  ++  can-connect
    |=  sole-id=@ta
    (team:title [our src]:bowl)
  ::
  ++  on-connect
    |=  sole-id=@ta
    [~ shoe]
  ::
  ++  on-disconnect
    |=  sole-id=@ta
    [~ shoe]
  --
::  +agent: creates wrapper core that handles sole events and calls shoe arms
::
++  agent
  |*  command-type=mold
  |=  =(shoe command-type)
  =|  state-0
  =*  state  -
  ^-  agent:gall
  =>
    |%
    ++  deal
      |=  cards=(list card)
      %+  turn  cards
      |=  =card
      ^-  card:agent:gall
      ?.  ?=(%shoe -.card)  card
      ?-  -.effect.card
          %sole
        =-  [%give %fact - %sole-effect !>(effect.effect.card)]
        %+  turn
          ?^  sole-ids.card  sole-ids.card
          ~(tap in ~(key by soles))
        |=  sole-id=@ta
        /sole/[sole-id]
      ::
          %table
        =;  fez=(list sole-effect)
          $(effect.card [%sole %mor fez])
        =,  +.effect.card
        :-  (row:draw & wide head)
        %+  turn  rows
        (cury (cury row:draw |) wide)
      ::
          %row
        $(effect.card [%sole (row:draw | +.effect.card)])
      ==
    --
  ::
  |_  =bowl:gall
  +*  this  .
      og    ~(. shoe bowl)
  ::
  ++  on-init
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  shoe  on-init:og
    [(deal cards) this]
  ::
  ++  on-save   !>([%shoe-app on-save:og state])
  ::
  ++  on-load
    |=  old-state=vase
    ^-  (quip card:agent:gall agent:gall)
    ::  we could be upgrading from a shoe-less app, in which case the vase
    ::  contains inner application state instead of our +on-save.
    ::  to distinguish between the two, we check for the presence of our own
    ::  +on-save tag in the vase.
    ::
    ?.  ?=([%shoe-app ^] q.old-state)
      =^  cards  shoe  (on-load:og old-state)
      [(deal cards) this]
    =^  old-inner  state  +:!<([%shoe-app vase state-0] old-state)
    =^  cards      shoe   (on-load:og old-inner)
    [(deal cards) this]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card:agent:gall agent:gall)
    ?.  ?=(%sole-action mark)
      =^  cards  shoe  (on-poke:og mark vase)
      [(deal cards) this]
    ::
    =/  act  !<(sole-action vase)
    =*  sole-id  id.act
    =/  cli-state=sole-share
      (~(gut by soles) sole-id *sole-share)
    |^  =^  [cards=(list card) =_cli-state]  shoe
          ?-  -.dat.act
            %det  (apply-edit +.dat.act)
            %clr  [[~ cli-state] shoe]
            %ret  try-command
            %tab  [(tab +.dat.act) shoe]
          ==
        :-  (deal cards)
        this(soles (~(put by soles) sole-id cli-state))
    ::
    ++  effect
      |=  =sole-effect
      ^-  card
      [%shoe [sole-id]~ %sole sole-effect]
    ::
    ++  apply-edit
      |=  =sole-change
      ^+  [[*(list card) cli-state] shoe]
      =^  inverse  cli-state
        (~(transceive sole cli-state) sole-change)
      ::  res: & for fully parsed, | for parsing failure at location
      ::
      =/  res=(each (unit [run=? cmd=command-type]) @ud)
        %+  rose  (tufa buf.cli-state)
        (command-parser:og sole-id)
      ?:  ?=(%& -.res)
        ::  only auto-run eligible commands if they were typed out
        ::  (that is, not retrieved from command history)
        ::
        ?.  &(?=(^ p.res) run.u.p.res !?=(%set -.ted.sole-change))
          [[~ cli-state] shoe]
        (run-command cmd.u.p.res)
      :_  shoe
      ::  parsing failed
      ::
      ?.  &(?=(%del -.inverse) =(+(p.inverse) (lent buf.cli-state)))
        ::  if edit was somewhere in the middle, let it happen anyway
        ::
        [~ cli-state]
      ::  if edit was insertion at buffer tail, revert it
      ::
      =^  undo  cli-state
        (~(transmit sole cli-state) inverse)
      :_  cli-state
      :_  ~
      %+  effect  %mor
      :~  [%det undo]   ::  undo edit
          [%err p.res]  ::  cursor to error location
      ==
    ::
    ++  try-command
      ^+  [[*(list card) cli-state] shoe]
      =/  res=(unit [? cmd=command-type])
        %+  rust  (tufa buf.cli-state)
        (command-parser:og sole-id)
      ?^  res  (run-command cmd.u.res)
      [[[(effect %bel ~)]~ cli-state] shoe]
    ::
    ++  run-command
      |=  cmd=command-type
      ^+  [[*(list card) cli-state] shoe]
      =^  cards  shoe  (on-command:og sole-id cmd)
      ::  clear buffer
      ::
      =^  clear  cli-state  (~(transmit sole cli-state) [%set ~])
      =-  [[[- cards] cli-state] shoe]
      %+  effect  %mor
      :~  [%nex ~]
          [%det clear]
      ==
    ::
    ++  tab
      |=  pos=@ud
      ^-  (quip card _cli-state)
      =+  (get-id-cord:auto pos (tufa buf.cli-state))
      =/  needle=term
        (fall id %$)
      ::  autocomplete empty command iff user at start of command
      ::
      =/  options=(list (option:auto tank))
        (search-prefix:auto needle (tab-list:og sole-id))
      =/  advance=term
        (longest-match:auto options)
      =/  to-send=tape
        %-  trip
        (rsh [3 (met 3 needle)] advance)
      =/  send-pos=@ud
        %+  add  pos
        (met 3 (fall forward ''))
      =|  cards=(list card)
      ::  only render the option list if we couldn't complete anything
      ::
      =?  cards  &(?=(~ to-send) ?=(^ options))
        [(effect %tab options) cards]
      |-  ^-  (quip card _cli-state)
      ?~  to-send
        [(flop cards) cli-state]
      =^  char  cli-state
        (~(transmit sole cli-state) [%ins send-pos `@c`i.to-send])
      %_  $
        cards     [(effect %det char) cards]
        send-pos  +(send-pos)
        to-send   t.to-send
      ==
    --
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card:agent:gall agent:gall)
    ?.  ?=([%sole @ ~] path)
      =^  cards  shoe
        (on-watch:og path)
      [(deal cards) this]
    =*  sole-id  i.t.path
    ?>  (can-connect:og sole-id)
    =.  soles  (~(put by soles) sole-id *sole-share)
    =^  cards  shoe
      (on-connect:og sole-id)
    :_  this
    %-  deal
    :_  cards
    [%shoe [sole-id]~ %sole %pro & dap.bowl "> "]
  ::
  ++  on-leave
    |=  =path
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  shoe  (on-leave:og path)
    [(deal cards) this]
  ::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?.  =(/x/dbug/state path)  ~
    ``noun+(slop on-save:og !>(shoe=state))
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  shoe  (on-agent:og wire sign)
    [(deal cards) this]
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  shoe  (on-arvo:og wire sign-arvo)
    [(deal cards) this]
  ::
  ++  on-fail
    |=  [=term =tang]
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  shoe  (on-fail:og term tang)
    [(deal cards) this]
  --
::
++  draw
  |%
  ++  row
    |=  [bold=? wide=(list @ud) cols=(list dime)]
    ^-  sole-effect
    :-  %mor
    ^-  (list sole-effect)
    =/  cows=(list [wid=@ud col=dime])
      %-  head
      %^  spin  cols  wide
      |=  [col=dime wiz=(list @ud)]
      ~|  [%too-few-wide col]
      ?>  ?=(^ wiz)
      [[i.wiz col] t.wiz]
    =/  cobs=(list [wid=@ud (list tape)])
      (turn cows col-as-lines)
    =+  [lin=0 any=|]
    =|  fez=(list sole-effect)
    |-  ^+  fez
    =;  out=tape
      ::  done when we're past the end of all columns
      ::
      ?:  (levy out (cury test ' '))
        (flop fez)
      =;  fec=sole-effect
        $(lin +(lin), fez [fec fez])
      ?.  bold  txt+out
      klr+[[`%br ~ ~]^[(crip out)]~]~
    %+  roll  cobs
    |=  [[wid=@ud lines=(list tape)] out=tape]
    %+  weld  out
    %+  weld  ?~(out "" " ")
    =+  l=(swag [lin 1] lines)
    ?^(l i.l (reap wid ' '))
  ::
  ++  col-as-lines
    |=  [wid=@ud col=dime]
    ^-  [@ud (list tape)]
    :-  wid
    %+  turn
      (break wid (col-as-text col) (break-sets -.col))
    (cury (cury pad wid) (alignment -.col))
  ::
  ++  col-as-text
    |=  col=dime
    ^-  tape
    ?+  p.col  (scow col)
      %t       (trip q.col)
      %tas     ['%' (scow col)]
    ==
  ::
  ++  alignment
    |=  wut=@ta
    ^-  ?(%left %right)
    ?:  ?=(?(%t %ta %tas %da) wut)
      %left
    %right
  ::
  ++  break-sets
    |=  wut=@ta
    ::  for: may break directly before these characters
    ::  aft: may break directly after these characters
    ::  new: always break on these characters, consuming them
    ::
    ^-  [for=(set @t) aft=(set @t) new=(set @t)]
    ?+  wut  [(sy " ") (sy ".:-/") (sy "\0a")]
      ?(%p %q)  [(sy "-") (sy "-") ~]
      %ux       [(sy ".") ~ ~]
    ==
  ::
  ++  break
    |=  [wid=@ud cot=tape brs=_*break-sets]
    ^-  (list tape)
    ~|  [wid cot]
    ?:  =("" cot)  ~
    =;  [lin=tape rem=tape]
      [lin $(cot rem)]
    ::  take snip of max width+1, search for breakpoint on that.
    ::  we grab one char extra, to look-ahead for for.brs.
    ::  later on, we always transfer _at least_ the extra char.
    ::
    =^  lin=tape  cot
      [(scag +(wid) cot) (slag +(wid) cot)]
    =+  len=(lent lin)
    ::  find the first newline character
    ::
    =/  new=(unit @ud)
      =+  new=~(tap in new.brs)
      =|  las=(unit @ud)
      |-
      ?~  new  las
      $(new t.new, las (hunt lth las (find [i.new]~ lin)))
    ::  if we found a newline, break on it
    ::
    ?^  new
      :-  (scag u.new lin)
      (weld (slag +(u.new) lin) cot)
    ::  if it fits, we're done
    ::
    ?:  (lte len wid)
      [lin cot]
    =+  nil=(flop lin)
    ::  search for latest aft match
    ::
    =/  aft=(unit @ud)
      ::  exclude the look-ahead character from search
      ::
      =.  len  (dec len)
      =.  nil  (slag 1 nil)
      =-  ?~(- ~ `+(u.-))
      ^-  (unit @ud)
      =+  aft=~(tap in aft.brs)
      =|  las=(unit @ud)
      |-
      ?~  aft  (bind las (cury sub (dec len)))
      $(aft t.aft, las (hunt lth las (find [i.aft]~ nil)))
    ::  search for latest for match
    ::
    =/  for=(unit @ud)
      =+  for=~(tap in for.brs)
      =|  las=(unit @ud)
      |-
      ?~  for  (bind las (cury sub (dec len)))
      =-  $(for t.for, las (hunt lth las -))
      =+  (find [i.for]~ nil)
      ::  don't break before the first character
      ::
      ?:(=(`(dec len) -) ~ -)
    ::  if any result, break as late as possible
    ::
    =+  brk=(hunt gth aft for)
    ?~  brk
      ::  lin can't break, produce it in its entirety
      ::  (after moving the look-ahead character back)
      ::
      :-  (scag wid lin)
      (weld (slag wid lin) cot)
    :-  (scag u.brk lin)
    =.  cot  (weld (slag u.brk lin) cot)
    ::  eat any leading whitespace the next line might have, "clean break"
    ::
    |-  ^+  cot
    ?~  cot  ~
    ?.  ?=(?(%' ' %'\09') i.cot)
      cot
    $(cot t.cot)
  ::
  ++  pad
    |=  [wid=@ud lyn=?(%left %right) lin=tape]
    ^+  lin
    =+  l=(lent lin)
    ?:  (gte l wid)  lin
    =+  p=(reap (sub wid l) ' ')
    ?-  lyn
      %left   (weld lin p)
      %right  (weld p lin)
    ==
  --
--
