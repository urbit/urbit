::  book: 2d console handler
::
::    every unique session is represented by a $book.
::    a book contains at least one $page.
::    a page contains at least one $pane.
::
::    this nested structure is reflected in the code: a nested "engine pattern"
::    is a very nice and natural fit for this.
::    ++  bo      ::  book engine
::      ++  pa    ::  page engine
::        ++  pn  ::  pane engine
::
::    panes contain exactly one of:
::    - %open splash screen,
::    - %pick agent picker,
::    - %dill connected agent,
::    - %logs scrollback reader TODO
::
::TODO  see select-layout section
::      https://www.man7.org/linux/man-pages/man1/tmux.1.html
::
::NOTE  thoughts on unicode support, (list @c), etc.
::      - for editing operations, yes, we certainly need a way to
::        index semantic "characters" as opposed to individual codepoints.
::        while sometimes decomposing by stripping codepoints is ok, it also
::        sometimes isn't!
::        https://manishearth.github.io/blog/2017/01/14/stop-ascribing-meaning-to-unicode-code-points/
::      - but for rendering operations, when operating on a character grid,
::        you _need_ to know the width of your characters.
::        but in a terminal context, whether this gets handled correctly or
::        not depends on the terminal emulator being used! so we actually
::        cannot strictly know whether a full-width character will eat into
::        two "character-cells" on the grid or not.
::        i see two approaches to solving this:
::        1) ship with a custom "dill emulator" and share display width
::           calculation logic between it and arvo.
::        2) punt on the problem entirely, never solve it in character grid
::           contexts, and simply say it's only solveable for ui protocol
::           client contexts.
::
::TODO  if async input handling is possible, do we want to make explicit the
::      end of processing an input, and track a queue of next inputs, instead
::      of handling those asap?
::      "Another thing is type-ahead. I remember in classic MacOS, people pressed shortcuts and started to type the filename or whatever. It was all perfectly recorded and replayed. In modern Windows, press Win-key and start to type, oops, it missed the first keypresses, presented the completely wrong results and made a mess of your workflow."
::      aka typeahead
::      https://news.ycombinator.com/item?id=30799608
::
/+  *etui, dbug, verb, default-agent
::
|%
+$  state-0
  $:  %0
      books=(map @ta book)
      ::TODO  maybe dills=(map gill:gall ?) to show if known compatible?
  ==
::
+$  book
  $:  focus=$@(@ud urge)
      shape=size
      pages=(list page)
      ::TODO  logs=(list @t) for on-demand viewing?
  ==
::
+$  urge
  $:  leaf=@ud  ::TODO  maybe nonce instead?
      goal=?(%$ %':' %rename-page)
      line=@t
  ==
::
+$  goal
  $%  [%rename-page title=@t]
  ==
::
+$  page
  $:  nonce=@ta
      title=@t
      shape=size
      focus=@ud  ::  selected pane
      panes=(list pane)  ::NOTE  actually (lest) in practice, but unergonomic
      style=plan
  ==
::
+$  plan
  ::TODO  could preserve %user always...
  $~  [%tall ~]
  $%  [%tall ~]
      [%solo orig=plan]
      [%wide ~]
      [%user lout=lay:splits:za]
  ==
::
+$  pane
  $:  nonce=@ta
            item  :: story=item
  ==
::
+$  item
  $%  [%open ~]
      [%pick pane-picker]
      [%logs log=(list stub)]  ::  latest first
      [%dill app=gill:gall ack=(unit ?) dill-buffer]
  ==
::
+$  pane-picker  ::  connection selector
  $:  cas=(list [=desk =dude:gall])
      pos=[pin=@ud pew=@ud]  ::  selected item & viewport location
      who=ship
  ==
::
::  terminal emulator behavior is such that, when a line is wrapped,
::  drawing on top of that line doesn't break the wrapping behavior.
::  iow, it's an edit-in-place, instead of making it count as a separate
::  hard-wrapped line.
::  as such, we track a bottom-first (list stub) per *semantic* line,
::  where each entry in the list is a soft-wrapped *visual* line. we also
::  track the width at which the soft-wrapping occurred, so we know if/when
::  to recalculate. this lets us modify these things in-place, and also saves
::  us from re-wrapping on every re-render.
::TODO  right now a lin=~ means an empty line.
::      do we want that to be lin=~[~] instead? ie lin=(lest stub)?
::      that way we don't have to account for the ~ case specially.
::      this also came up in the text editor...
::
+$  dill-buffer
  $:  bac=(list [wid=@ud lin=(list stub)])              ::  scrollback lines
      pos=spot                                          ::  cursor
    ::
      $=  way  %-  unit  $:                             ::  scrolling?
        ::TODO  add cursor, use +generic-scroll:etui
        pus=@ud                                         ::  scroll position
        que=(list blit)                                 ::  rev. pending blits
      ==
  ==
::
+$  spot  [x=@ud y=@ud]  ::  graph coordinates
+$  size  [w=@ud h=@ud]  ::  including origin
+$  zone  [spot size]    ::  bounding box
::
+$  card  card:agent:gall
+$  blit  blit:dill
::
++  config
  |%
  ++  colors
    |%
    ++  lines  ^-  stye  ``%g
    --
  ++  keys
    |%
    ::  prefix: general key command prefix to all other keys
    ::
    ++  prefix  [%mod %ctl ~-x]
    ::  longer: enter textual command mode
    ::
    ++  longer  ':'
    ::  create: new page
    ::  delete: remove focussed page
    ::  next:   focus next page
    ::  prev:   focus previous page
    ::  style:  switch layout style
    ::  solo:   full-screen focussed pane
    ::  choose: focus page by index
    ::
    ++  create  'C'
    ++  delete  'X'
    ++  next    'N'
    ++  prev    'P'
    ++  style   's'
    ++  solo    'z'
    ++  rename  ','
    ++  choose  ['0' '9']
    ::  split:  new pane
    ::  close:  remove focussed pane
    ::  fore:   focus next pane
    ::  prev:   focus previous pane
    ::  spin:   rotate pane order forward
    ::  scroll: enter scroll mode
    ::
    ++  split   'c'
    ++  hori    '"'
    ++  vert    '%'
    ++  close   'x'
    ++  fore    'n'
    ++  back    'p'
    ++  spin    '>'  ::TODO  implement
    ++  scroll  '['
    --
  ++  dill
    |%
    ++  history  500
    --
  --
--
::
=|  state-0
=*  state  -
%-  agent:dbug
%+  verb  |
^-  agent:gall
::
=<
  |_  =bowl:gall
  +*  this  .
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init  [~ this]
  ++  on-save  !>(state)
  ::
  ++  on-load
    |=  ole=vase
    =.  state  !<(state-0 ole)
    =/  sez=(list @ta)  ~(tap in ~(key by books))
    =|  caz=(list card)
    |-
    ?~  sez  [caz this]
    =^  cas  books  bo-abet:(bo-belt:(bo-apex:bo i.sez bowl) [%hey ~])
    $(caz (weld caz cas), sez t.sez)
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?.  ?=(%dill-poke mark)  (on-poke:def mark vase)
    =+  !<([ses=@ta belt=dill-belt:dill] vase)
    ::TODO  should dill send %rez before %hey?
    =^  caz  books  bo-abet:(bo-belt:(bo-apex:bo ses bowl) belt)
    [caz this]
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+  wire  (on-agent:def wire sign)
        [%book book=@ta *]
      =^  caz  books
        bo-abet:(bo-sign:(bo-apex:bo i.t.wire bowl) t.t.wire sign)
      [caz this]
    ==
  ::
  ++  on-watch
    |=  =path
    ?.  ?=([%dill @ ~] path)  (on-watch:def path)
    [~ this]  :: [~ this(books (~(put in books) i.t.path))]
  ::
  ++  on-leave
    |=  =path
    ?.  ?=([%dill @ ~] path)  (on-leave:def path)
    ::TODO  time out sessions after a week(?) of inactivity
    [~ this]  :: [~ this(books (~(del in books) i.t.path))]
  ::
  ++  on-peek   on-peek:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|%
++  db-pane  ::  stub pane contents for debug prints
  |=  =pane
  ^+  pane
  ?+  +<.pane  pane
    %logs  pane(log ~)
    %dill  pane(bac ~)
  ==
::  +bo: book engine
::
++  bo
  =|  biz=(list blit)
  =|  caz=(list card)
  =|  hop=_|
  |_  [ses=@ta bok=book =bowl:gall]
  +*  bo  .
  ::
  ::  +|  %gangway
  ::
  ++  bo-apex
    |=  [s=@ta b=bowl:gall]
    ^+  bo
    =;  blank=page
      bo(ses s, bok (~(gut by books) s [0 [80 24] [blank]~]), bowl b)
    ::TODO  deduplicate with blank:bo-task
    ::TODO  if there's any risk of creating multiple pages/panes per event,
    ::      then we must add pages length or something to the seed.
    :*  (scot %uv (end 5 (shas %page eny.bowl)))
        'page'
        [80 23]  ::TODO  bad factoring? send %rez?
        0
        [(scot %uv (end 5 (shas %pane eny.bowl))) %open ~]~
        [%tall ~]
    ==
  ::
  ++  bo-abet
    ^-  (quip card _books)
    =?  bo  &  :: kinda want this, but +di-blit is difficult: |(hop !=(~ biz))
      bo-cure
    :_  (~(put by books) ses bok)
    ?~  biz  (flop caz)
    =;  bit=card
      [bit (flop caz)]
    [%give %fact [/dill/[ses]]~ %dill-blit !>(`blit`[%mor (flop biz)])]
  ::
  ::  +|  %enqueue
  ::
  ++  bo-blit  |=  bit=blit         bo(biz [bit biz])
  ++  bo-blis  |=  bis=(list blit)  bo(biz (weld (flop bis) biz))
  ++  bo-emit  |=  cad=card         bo(caz [cad caz])
  ++  bo-emil  |=  cas=(list card)  bo(caz (weld (flop cas) caz))
  ::
  ++  bo-dilt
    |=  [wir=wire app=gill:gall sid=@tas bet=dill-belt:dill]
    (bo-emit %pass wir %agent app %poke %dill-poke !>([sid bet]))
  ::
  ::  +|  %process
  ::
  ++  bo-sign  ::  on agent event
    |=  [=wire =sign:agent:gall]
    ^+  bo
    ?+  wire  ~|([%book %strange-wire ses wire] !!)
        [%page page=@ta *]
      =/  pa  (pa-apex:pa =(nonce:bo-fag i.t.wire) i.t.wire)
      pa-abet:(pa-sign:pa t.t.wire sign)
    ==
  ::
  ++  bo-belt  ::  on user event
    |=  belt=dill-belt:dill
    ^+  bo
    ::  resize events always go to the selected page,
    ::  others get the resize event as soon as they're viewed
    ::
    ?:  ?=([%rez *] belt)
      ?:  =(shape.bok +.belt)  bo
      =.  shape.bok  +.belt
      ::NOTE  account for status bar, which is part of +bo
      bo-foot:pa-abet:(pa-size:bo-pag [p.belt (dec q.belt)])
    ::  other dill-level belts
    ::
    ?:  ?=([%hey ~] belt)
      bo-full
    ?:  ?=([%yow *] belt)
      ~&  [%yow +.belt]
      bo  ::TODO  set up?
    ?:  ?=([%cru *] belt)
      ~&  [%cru +.belt]
      bo  ::TODO  show error somewhere?
    ::  when pressing the prefix, switch focus to self
    ::  unless we already have focus, then send the prefix as-is.
    ::
    ?:  =(belt prefix:keys:config)
      ?:  ?=(@ focus.bok)
        bo-heel(focus.bok [focus.bok %$ ''])
      bo-heel:pa-abet:(pa-belt:bo-pag(focus.bok leaf.focus.bok) belt)
    ::  handle clicks on the status bar
    ::
    ?:  ?&  ?=([%hit *] belt)
            =(y.belt (dec h.shape.bok))
            !=(%sole -.style:bo-fag)  ::TMP
        ==
      ~&  %uhh-click
      bo
    ::  if focus is in a page, send input there
    ::
    ?@  focus.bok  pa-abet:(pa-belt:bo-pag belt)
    (bo-task belt)
  ::
  ++  bo-read
    |=  [aim=@tas job=@t]
    ^+  bo
    =?  job  !=(':' aim)  (rap 3 aim ' ' job ~)
    ?>  ?=(^ focus.bok)
    =;  gol=(unit goal)
      ?~  gol  (bo-blit [%bel ~])
      (bo-work(focus.bok leaf.focus.bok) u.gol)
    %+  rush  job
    ;~  pose
      ;~((glue ace) (perk %rename-page ~) (cook crip (star next)))
    ==
  ::
  ++  bo-work
    |=  =goal
    ^+  bo
    |^  ?-  -.goal
          %rename-page  (dunk |=(p=page p(title title.goal)))
        ==
    ::
    ++  dunk
      |=  gate=$-(page page)
      =-  bo(pages.bok -)
      %^  prod  pages.bok
        ?@(focus.bok focus.bok leaf.focus.bok)
      gate
    --
  ::
  ++  bo-task  ::  on book command
    |=  belt=belt:dill
    ^+  bo
    =<  bo-foot
    ?>  ?=(^ focus.bok)
    =,  focus.bok
    ?.  =(%$ goal)
      ::  we're in command line mode, handle input
      ::
      ?+  belt  (bo-blit [%bel ~])
        @         bo(line.focus.bok (cat 3 line.focus.bok belt))
        [%txt *]  bo(line.focus.bok (cat 3 line.focus.bok (rap 3 +.belt)))
        [%ret *]  (bo-read [goal line]:focus.bok)
      ::
          [%bac ~]  ::TODO  revisit behavior
        =/  m=@ud  (met 3 line.focus.bok)
        ?:  =(0 m)  (bo-blit [%bel ~])
        bo(line.focus.bok (end 3^(dec m) line.focus.bok))
      ==
    ::
    |^  =*  k  keys:config
        ?:  =(belt longer:k)  longer
        ?:  =(belt rename:k)  rename
        ?:  =(belt create:k)  create
        ?:  =(belt delete:k)  delete
        ?:  =(belt next:k)    next
        ?:  =(belt prev:k)    prev
        ?:  ?&(?=(@ belt) (gte belt -:choose:k) (lte belt +:choose:k))
          (choose (sub belt -:choose:k))
        ::
        =<  bo(focus.bok ?@(focus.bok focus.bok leaf.focus.bok))
        ?:  =(belt solo:k)    solo
        ?:  =(belt style:k)   style
        ?:  =(belt split:k)   split
        ?:  =(belt hori:k)    hori
        ?:  =(belt vert:k)    vert
        ?:  =(belt close:k)   close
        ?:  =(belt fore:k)    fore
        ?:  =(belt back:k)    back
        ?:  =(belt scroll:k)  scroll
        (bo-blit [%bel ~])
    ::
    ++  longer
      bo(goal.focus.bok %':')
    ::
    ++  rename
      bo(goal.focus.bok %rename-page)
    ::
    ++  create
      %=  bo-body
        focus.bok  (lent pages.bok)
        pages.bok  (snoc pages.bok blank)
      ==
    ::
    ++  delete
      =.  bo  =<(?>(?=(^ focus.bok) .) pa-nuke:bo-pag)
      =?  pages.bok  =(~ pages.bok)  [blank]~
      bo-body(focus.bok (min leaf (dec (lent pages.bok))))
    ::
    ++  next
      =/  next=@ud
        (mod +(leaf) (lent pages.bok))
      ?:  =(next leaf)  bo
      =<  pa-abet
      %-  pa-spot:bo-pag(hop &, focus.bok next)
      ::NOTE  account for status bar, which is part of +bo
      ::TMP  don't show status bar in solo mode
      =?  h.shape.bok  =(%solo -.style:bo-fag)  +(h.shape.bok)
      [w.shape.bok (dec h.shape.bok)]
    ::
    ++  prev
      =/  next=@ud
        (dec ?:(=(0 leaf) (lent pages.bok) leaf))
      ?:  =(next leaf)  bo
      =<  pa-abet
      %-  pa-spot:bo-pag(hop &, focus.bok next)
      ::NOTE  account for status bar, which is part of +bo
      ::TMP  don't show status bar in solo mode
      =?  h.shape.bok  =([%solo *] style:bo-fag)  +(h.shape.bok)
      [w.shape.bok (dec h.shape.bok)]
    ::
    ++  choose
      |=  next=@ud
      ?:  (gte next (lent pages.bok))
        ::TODO  could display message?
        (bo-blit(focus.bok leaf.focus.bok) [%bel ~])
      =<  pa-abet
      %-  pa-spot:bo-pag(hop &, focus.bok next)
      ::NOTE  account for status bar, which is part of+bo
      ::TMP  don't show status bar in solo mode
      =?  h.shape.bok  =(%solo -.style:bo-fag)  +(h.shape.bok)
      [w.shape.bok (dec h.shape.bok)]
    ::
    ++  style   =~  bo-pag  pa-task  style       pa-abet  ==
    ++  solo    =~  bo-pag  pa-task  solo        pa-abet  ==
    ++  split   =~  bo-pag  pa-task  split       pa-abet  ==
    ++  hori    =~  bo-pag  pa-task  (splay %h)  pa-abet  ==
    ++  vert    =~  bo-pag  pa-task  (splay %v)  pa-abet  ==
    ++  close   =~  bo-pag  pa-task  close       pa-abet  ==
    ++  fore    =~  bo-pag  pa-task  fore        pa-abet  ==
    ++  back    =~  bo-pag  pa-task  back        pa-abet  ==
    ++  scroll  =~  bo-pag  pa-task  scroll      pa-abet  ==
    ::
    ++  blank
      ^-  page
      :*  (scot %uv (end 5 (shas %page eny.bowl)))
          'page'
          [w.shape.bok (dec h.shape.bok)]  ::TODO  bad factoring? send %rez?
          0
          [(scot %uv (end 5 (shas %pane eny.bowl))) %open ~]~
          [%tall ~]
      ==
    --
    ::TODO  when switching pages, if !=(shape.bok shape.pag), pa-size & bo-body
  ::
  ::  +|  %display
  ::
  ++  bo-full  =~(bo-body bo-foot)
  ::
  ++  bo-body  ::  draw book body (page)
    ::TODO  selective redraw
    pa-abet:pa-full:bo-pag
  ::
  ++  bo-foot  ::  draw book footer/status bar
    ^+  bo
    ::TMP  don't show status bar in solo mode
    ?:  =(%solo -.style:bo-fag)  bo
    ?.  &(?=(^ focus.bok) !=(%$ goal.focus.bok))
      =~(bo-heel bo-toes bo-sole)
    =/  input=(list @)
      (trip (cat 3 bo-gol line.focus.bok))
    =/  wider=(list @)
      (weld input (reap (sub w.shape.bok (lent input)) ~-.))
    %-  bo-blis
    :~  [%hop 0 (dec h.shape.bok)]
        [%klr [~ %k %w]^wider ~]
    ==
  ::
  ++  bo-heel  ::  left-most status
    ^+  bo
    ?:  =(%solo -.style:bo-fag)  bo
    =/  heel=tour
      ::TODO  maybe ^~  %-  tuba  instead, for readability?
      ?@  focus.bok
        [~-~2591. ~-~2591. ~-~2591. ~-~2503. ~-. ~]
      [~-~2593. ~-~2593. ~-~2593. ~-~2503. ~-. ~]
    %-  bo-blis
    :~  [%hop 0 (dec h.shape.bok)]
        [%klr [~ %k %w]^heel ~]
    ==
  ::
  ++  bo-sole  ::  center status
    ^+  bo
    =/  etc=stub
      =|  i=@ud
      |-
      ?~  pages.bok  ~
      :_  [[[~ %k %w] ~-. ~] $(pages.bok t.pages.bok, i +(i))]
      :-  :_  [%k %w]
          ?.  =(i focus.bok)  ~  ::TODO  ?=(^ focus) case?
          (sy %br %un ~)
      `(list @)`"{(a-co:^co i)}:{(trip title.i.pages.bok)}"
    =/  sole=stub
      ::TODO  5 is from +bo-heel, should probably not be hardcoded
      =/  wid=@ud  (sub w.shape.bok 5)
      %^  side:klr  %c
        [wid [~ %k %w] ~-.]
      (scag:klr wid etc)
    %-  bo-blis
    :~  [%hop 5 (dec h.shape.bok)]
        [%klr sole]
    ==
  ::
  ++  bo-toes  ::  right-most status
    ^+  bo
    ::TODO  kinda want to draw a clock or something, but many hard
    ::      questions there. it needs to stay up to date, not draw on
    ::      top of/under +bo-sole, etc.
    bo
    :: =/  toes=tour
    ::   :+  ~-~2503.  ' '
    ::   (scow %da (sub now.bowl (mod now.bowl ~d1)))
    :: :~  [%hop (sub w.shape.bok (lent toes)) (dec h.shape.bok)]
    ::     [%klr [~ %k %w]^toes ~]
    :: ==
  ::
  ++  bo-cure  ::  move cursor to focussed position
    ?@  focus.bok
      pa-abet:pa-cure:bo-pag
    %+  bo-blit  %hop
    [(add (met 3 bo-gol) (met 3 line.focus.bok)) (dec h.shape.bok)]
  ::
  ++  bo-gol
    ?>  ?=(^ focus.bok)
    ?:  ?=(%':' goal.focus.bok)  ':'
    (rap 3 '(' goal.focus.bok ') ' ~)
  ::
  ::  +|  %page-ng
  ::
  ++  bo-fag
    ::TODO  what if no pages? it crashes lol, pls fix
    (snag ?@(focus.bok focus.bok leaf.focus.bok) pages.bok)
  ::
  ++  bo-pag  ::  focussed page engine
    ::TODO  the way in which this uses +pa-apex is awkward,
    ::      but not using it would be more fragile...
    (pa-apex:pa & nonce:bo-fag)
  ::
  ::  +pa: page engine
  ::
  ++  pa
    |_  [vis=? pog=@ta pag=page]
    +*  pa    .
        :: page  `^page`+<+>  ::NOTE  scary broken
    ::
    ::  +|  %gangway
    ::
    ++  pa-apex
      |=  [v=? p=@ta]
      =/  pages  pages.bok
      |-  ^+  pa
      ?~  pages  ~|([%book %missing-page ses=ses page=p] !!)
      ?:  =(p nonce.i.pages)
        pa(pog p, pag i.pages, vis v)
      $(pages t.pages)
    ::
    ++  pa-abet
      ^+  bo
      =;  =_pages.bok  bo(pages.bok pages)
      =/  pages  pages.bok
      |-
      ?~  pages  [pag]~
      ?:  =(pog nonce.i.pages)
        [pag t.pages]
      [i.pages $(pages t.pages)]
    ::
    ++  pa-nuke
      |-  ^+  bo
      =.  pa
        =/  panes  panes.pag
        |-
        ?~  panes  pa
        =.  pa  pn-nuke:(pn-apex:pn nonce.i.panes)
        $(panes t.panes)
      ::TODO  terrifyingly similar to +pa-abet
      =;  =_pages.bok  bo(pages.bok pages)
      =/  pages  pages.bok
      |-
      ?~  pages  [pag]~
      ?:  =(pog nonce.i.pages)  t.pages
      [i.pages $(pages t.pages)]
    ::
    ::  +|  %process
    ::
    ++  pa-sign  ::  on agent event
      |=  [=wire =sign:agent:gall]
      ^+  pa
      ?+  wire  ~|([%book %strange-wire ses pog wire] !!)
          [%pane pane=@ta *]
        =/  pn  (pn-apex:pn i.t.wire)
        pn-abet:(pn-sign:pn t.t.wire sign)
      ==
    ::
    ++  pa-size
      |=  =size
      ^+  pa
      ?:  =(size shape.pag)  pa
      =.  shape.pag  size
      pa-full:pa-melt
    ::
    ++  pa-spot  ::  come into view  ::TODO  dedupe with +pa-size?
      |=  =size
      ^+  pa
      ?:  =(size shape.pag)  pa-full
      =.  shape.pag  size
      pa-full:pa-melt
    ::
    ++  pa-melt  ::  relayout
      =/  zones=(list zone)  pa-zos
      =/  panes=(list pane)  panes.pag
      :: ~&  [z=(lent zones) p=(lent panes)]
      :: ~&  [zones=zones panes=(turn panes db-pane)]
      |-
      ?~  panes  pa
      ?>  ?=(^ zones)
      =.  pa  pn-abet:pn-size:(pn-apex:pn nonce.i.panes)
      $(panes t.panes, zones t.zones)
    ::
    ++  pa-belt  ::  on user event
      |=  belt=belt:dill
      ^+  pa
      ?.  ?=([%hit *] belt)
        pn-abet:(pn-belt:pa-pan belt)
      ::  if clicked, switch focus first
      ::
      ?:  =(%solo -.style.pag)
        pn-abet:(pn-belt:(pn-apex:pn nonce:(snag focus.pag panes.pag)) belt)
      =/  panes=(list pane)  panes.pag
      =/  zones=(list zone)  pa-zos
      =/  i=@ud              0
      |-
      ?~  panes  ~&(%miss pa)
      ?>  ?=(^ zones)
      =,  i.zones
      ?.  ?&  (gte x.belt x)  (lth x.belt (add x w))
              (gte y.belt y)  (lth y.belt (add y h))
          ==
        $(i +(i), panes t.panes, zones t.zones)
      =?  pa  !=(focus.pag i)
        pa-line(hop &, focus.pag i)
      ::TODO  should we really send click right away,
      ::      if it didn't previously have focus?
      pn-abet:(pn-belt:(pn-apex:pn nonce.i.panes) belt)
    ::
    ++  pa-task
      |%
      ++  style
        =;  =_style.pag
          =.  style.pag  style
          ::TODO  when would the non-vis case happen?
          ::      we might still want to melt there...
          ?.  vis  pa
          pa-full:pa-melt
        |-  ?-  -.style.pag
          %tall  [%wide ~]
          %wide  [%tall ~]
          %solo  [%solo $(style.pag orig.style.pag)]
          %user  ?>  ?=(^ lout.style.pag)
                 ?-  w.lout.style.pag
                   %h  [%tall ~]
                   %v  [%wide ~]
                 ==
        ==
      ::
      ++  solo
        =;  =_style.pag
          =.  style.pag  style
          ::TODO  when would the non-vis case happen?
          ::      we might still want to melt there...
          ?.  vis  pa
          pa-full:pa-melt
        ?:  ?=(%solo -.style.pag)
          orig.style.pag
        [%solo style.pag]
      ::
      ++  split
        =/  next=@ud   +(focus.pag)
        =.  focus.pag  next
        =.  panes.pag  (into panes.pag next blank)
        pa-full:pa-melt
      ::
      ++  splay
        |=  way=?(%h %v)
        ~&  way+way
        ~&  >  [%fore style.pag]
        =<  ~&  >  [%aftr style.pag]
            .
        ::  next: if 1, split here. if 0, split has happened
        ::
        =/  next=@ud   +(focus.pag)
        =.  style.pag
          =/  ind=@ud  focus.pag
          =/  =lay:splits:za
            |-
            ?-  -.style.pag
              %tall  [%v =+(l=(turn panes.pag |=(* ~)) ?>(?=(^ l) l))]
              %wide  [%h =+(l=(turn panes.pag |=(* ~)) ?>(?=(^ l) l))]
              %solo  $(style.pag orig.style.pag)
              %user  lout.style.pag
            ==
          ~&  [%layt lay]
          ::NOTE  no, i don't get it either...
          ::TODO  this needs some serious re-work. start with simple cases!
          =;  [n=@ud l=_lay]
            ?:  !=(0 n)  ~&(%woah ~|(%woah !!))
            [%user l]
          |-  ^-  [@ud _lay]
          ~&  next+next
          ?@  lay
            ?:  =(0 next)  [0 lay]
            ?:  =(1 next)
              ~&  %insert-way-a
              [0 [way ~[lay ~]]]
            [(dec next) lay]
          |-  ^-  [@ud _lay]
          ::  seek inside branch, return [n maybe-modified-branch]
          =^  next  i.l.lay  ^$(lay i.l.lay)
          ~&  nuxt+next
          =.  ^next  next
          ?:  =(0 next)
            [0 lay]
            :: :-  0
            :: ?:  =(way w.lay)
            ::   ~&  %same-way-0
            ::   [w.lay ~ l.lay]  ::TODO  not snoc?
            :: ?~  t.l.lay
            ::   ~&  %insert-way-0-a
            ::   [way i.l.lay ~ ~]
            :: ~&  %insert-way-0-b
            :: [w.lay [way i.l.lay ~ ~] t.l.lay]
          ?:  =(1 next)
            :-  0
            ?~  t.l.lay  ~&  %insert-way-b  [way ~[i.l.lay ~]]
            ?:  =(way w.lay)
              ~&  %same-way
              [w.lay i.l.lay ~ t.l.lay]
            ~&  %insert-way-c
            [w.lay i.l.lay [way ~ t.l.lay] ~]
          ?~  t.l.lay  [next lay]
          ::TODO  this feels idiotic
          =/  [n=@ud l=lay:splits:za]
            $(l.lay t.l.lay)
          ?>  ?=(^ l)
          $(next n, lay [w.lay i.l.lay t.l.l])
        =.  focus.pag  next
        =.  panes.pag  (into panes.pag next blank)
        pa-full:pa-melt
      ::
      ++  close
        =?  style.pag  ?=(%user -.style.pag)
          ::TODO  need to dedupe with search logic in +splay...
          !!
        =.  pa  pn-nuke:pa-pan
        =?  panes.pag  =(~ panes.pag)  [blank]~
        =.  focus.pag  (min focus.pag (dec (lent panes.pag)))
        pa-full:pa-melt
      ::
      ++  fore
        =/  next=@ud
          (mod +(focus.pag) (lent panes.pag))
        ?:  =(next focus.pag)  pa
        =.  hop        &
        =.  focus.pag  next
        ?:(=(%solo -.style.pag) pa-body pa-line)
      ::
      ++  back
        =/  next=@ud
          (dec ?:(=(0 focus.pag) (lent panes.pag) focus.pag))
        ?:  =(next focus.pag)  pa
        =.  hop        &
        =.  focus.pag  next
        ?:(=(%solo -.style.pag) pa-body pa-line)
      ::
      ++  scroll
        pn-abet:pn-roll:pa-pan
      ::
      ++  blank
        ^-  pane
        ::TODO  duplicated with +blank of +bo
        [(scot %uv (end 5 (shas %pane eny.bowl))) %open ~]
      --
    ::
    ::  +|  %display
    ::
    ++  pa-full  =~(pa-body pa-line)
    ::
    ++  pa-body  ::  panes
      ?:  =(%solo -.style.pag)  ::NOTE  tmi
        pn-abet:pn-full:pa-pan
      =/  panes=(list pane)  panes.pag
      |-
      ?~  panes  pa
      =.  pa  pn-abet:pn-full:(pn-apex:pn nonce.i.panes)
      $(panes t.panes)
    ::
    ++  pa-line  ::  borders
      =-  pa(bo (bo-blis -))
      =/  pas=@ud  (lent panes.pag)
      =/  poz=(list zone)  pa-zos
      |^  ?-  -.style.pag
            %solo  solo
            %wide  wide
            %tall  tall
            %user  user
          ==
      ::
      ++  show
        |=  l=tour
        ^-  blit
        [%klr [lines:colors:config l]~]
      ::
      ++  hori  ::TODO  improve interface, move into etui
        |=  [l=@c [w=@ud c=@c] r=@c]
        (show l (snoc (reap w c) r))
      ::
      ++  vert  ::TODO  improve interface, move into etui
        |=  [spot z=(list blit)]
        |=  [t=@c [h=@ud c=@c] b=@c]
        ::NOTE  performance?
        =/  e=@ud  (add y h)
        :+  [%hop x y]
          (show t ~)
        |-
        =.  y  +(y)
        ?:  =(e y)
          :+  [%hop x y]
            (show b ~)
          z
        :+  [%hop x y]
          (show c ~)
        $
      ::
      ++  user
        =<  b
        %+  roll
          ::  focussed pane always first, so we can draw it on top easily
          ::
          ^-  (list zone)
          :-  (snag focus.pag pa-zos)
          (oust [focus.pag 1] pa-zos)
        |=  [z=zone b=(list blit) c=@ud]
        =;  o=blit:dill
          ?>  ?=(%mor -.o)
          ::  weld order here to draw first one on top
          [(weld p.o b) +(c)]
        %^  ~(line zi (grow:za z))
            lines:colors:config
          ~
        =/  f=?  =(c 0)
        ::TODO  we could use the real coordinates of the zone, paired with
        ::      the size of the page, to deduce which corners we need, right?
        ::      (landlocked ones will still be crosses though...)
        ?:  f  [~-~2501. ~-~2503. ~-~254b.]
        [~-~2500. ~-~2502. ~-~253c.]
      ::
      ++  solo
        ^-  (list blit)
        ::TMP  don't show status bar in solo mode
        =.  h.shape.pag  +(h.shape.pag)
        =;  o=blit:dill
          ?>(?=(%mor -.o) +.o)  ::TODO  ugly
        %^  ~(line zi [0 0] shape.pag)  lines:colors:config
          ~  ::[%b ~ ~]
        [[~-~2550. ~-~2551.] ~-~2554. ~-~2557. ~-~255a. ~-~255d.]
      ::
      ++  wide
        =<  b
        %+  roll  pa-zos
        |=  [z=zone b=(list blit) c=@ud]
        ::TODO  +pa-zos removed border space, this re-adds it... refactor?
        =;  o=blit:dill
          ?>  ?=(%mor -.o)
          [(weld p.o b) +(c)]
        ::TODO  there's probably some ridiculous way to deduplicate this
        ::      with +tall
        %^  ~(line zi (grow:za z))
            lines:colors:config
          ?:(=(0 c) ~ [%t ~ ~])
        =/  f=?  =(c focus.pag)
        =/  n=?  =(+(c) focus.pag)
        :-  :*  ?:  f       ~-~2501.  ~-~2500.  ::  ──
                ?:  |(f n)  ~-~2501.  ~-~2500.  ::  ──
                ?:  f       ~-~2503.  ~-~2502.  ::  │
                ?:  f       ~-~2503.  ~-~2502.  ::  │
            ==
        :*  ?.  =(0 c)  *@c
            ?:(f ~-~250f. ~-~250c.)             ::  ┌─
          ::
            ?.  =(0 c)  *@c
            ?:(f ~-~2513. ~-~2510.)             ::  ─┐
          ::
            ?:  =((dec pas) c)
              ?:(f ~-~2517. ~-~2514.)           ::  └─
            ?:  f  ~-~2521.                     ::  ┡━
            ?:  n  ~-~2522.                     ::  ┢━
                   ~-~251c.                     ::  ├─
          ::
            ?:  =((dec pas) c)
              ?:(f ~-~251b. ~-~2518.)           ::  ─┘
            ?:  f  ~-~2529.                     ::  ━┩
            ?:  n  ~-~252a.                     ::  ━┪
                   ~-~2524.                     ::  ─┤
        ==
      ::
      ++  tall
        =<  b
        %+  roll  pa-zos
        |=  [z=zone b=(list blit) c=@ud]
        ::TODO  +pa-zos removed border space, this re-adds it... refactor?
        =;  o=blit:dill
          ?>  ?=(%mor -.o)
          [(weld p.o b) +(c)]
        %^  ~(line zi (grow:za z))
            lines:colors:config
          ?:(=(0 c) ~ [%l ~ ~])
        =/  f=?  =(c focus.pag)
        =/  n=?  =(+(c) focus.pag)
        :-  :*  ?:  f       ~-~2501.  ~-~2500.  ::  ──
                ?:  f       ~-~2501.  ~-~2500.  ::  ──
                ?:  f       ~-~2503.  ~-~2502.  ::  │
                ?:  |(f n)  ~-~2503.  ~-~2502.  ::  │
            ==
        :*  ?.  =(0 c)  *@c
            ?:(f ~-~250f. ~-~250c.)             ::  ┌─
          ::
            ?:  =((dec pas) c)
              ?:(f ~-~2513. ~-~2510.)           ::  ─┐
            ?:  f  ~-~2531.                     ::  ━┱─
            ?:  n  ~-~2532.                     ::  ─┲━
                   ~-~252c.                     ::  ─┬─
          ::
            ?.  =(0 c)  *@c
            ?:(f ~-~2517. ~-~2514.)             ::  └─
          ::
            ?:  =((dec pas) c)
              ?:(f ~-~251b. ~-~2518.)           ::  ─┘
            ?:  f  ~-~2539.                     ::  ━┹─
            ?:  n  ~-~253a.                     ::  ─┺━
                   ~-~2534.                     ::  ─┴─
        ==
      --
    ::
    ++  pa-cure
      pn-abet:pn-cure:pa-pan
    ::
    ::  +|
    ::
    ++  pa-zos
      ::TODO  maybe use splits:za:etui?
      =>  [siz=shape.pag pas=(lent panes.pag) lay=style.pag zone=zone za=za ..zuse]
      :: ~&  [%pa-zos pas=pas]
      ::TMP  don't show status bar in solo mode
      =?  h.siz  =(%solo -.lay)  +(h.siz)
      ^-  (list zone)  ~+
      =.  w.siz  (sub w.siz 2)  ::  all-round borders
      =.  h.siz  (sub h.siz 2)  ::  all-round borders
      |^  ?-  -.lay
              %solo
            (reap pas [[1 1] siz])
          ::
              %tall
            =-  (flop a)
            %+  roll  (calc w.siz)
            |=  [w=@ud [x=_1 a=(list zone)]]
            :-  (add x +(w))
            [[x^1 w^h.siz] a]
          ::
              %wide
            =-  (flop a)
            %+  roll  (calc h.siz)
            |=  [h=@ud [y=_1 a=(list zone)]]
            :-  (add y +(h))
            [[1^y w.siz^h] a]
          ::
              %user
            =-  ~&  [%pa-zos-user pas=pas out=(lent -) +.lay]  -
            %+  turn
              (splits:za [[1 1] +(w.siz) +(h.siz)] +.lay)
            |=(z=zone ~&(z+z z(w (dec w.z), h (dec h.z))))
          ==
      ::
      ++  calc
        |=  tot=@ud
        =/  per=@ud  (sub tot (dec pas))  ::  borders between panes
        =/  bas=@ud  (div per pas)        ::  min available per pane
        =/  rem=@ud  (mod per pas)        ::  remainder  ::TODO  dvr
        =/  diz      (reap pas bas)       ::  per pane
        |-                                ::  dole out the remainder
        ?:  =(0 rem)  diz
        ?>  ?=(^ diz)
        [+(i.diz) $(diz t.diz, rem (dec rem))]
      --
    ::
    ::  +|  %pane-ng
    ::
    ++  pa-fan
      (snag focus.pag panes.pag)
    ::
    ++  pa-pan
      ::TODO  this, too, is just a little bit awkward
      (pn-apex:pn nonce:pa-fan)
    ::
    ::  +pn: pane engine
    ::TODO  want engines per contents type? yes definitely. only question is:
    ::      do we just add ?- at every current etc:(pn-apex) callsite?
    ::      that'd make pane capabilities leak into +pa's logic,
    ::      which seems at least a little bit bad.
    ::
    ++  pn
      |_  [pon=@ta pan=pane zon=zone]
      +*  pn  .
      ::
      ::  +|  %gangway
      ::
      ++  pn-apex
        |=  p=@ta
        ::TODO  somehow, for new panes, should clear all contents.
        ::      we don't know what was in here before, and we would like
        ::      the luxury of not clearing where we have no content.
        =/  panes  panes.pag
        =/  zones  pa-zos
        |-  ^+  pn
        ?~  panes  ~|([%book %missing-pane ses=ses pane=p] !!)
        ?>  ?=(^ zones)
        ?:  =(p nonce.i.panes)
          pn(pon p, pan i.panes, zon i.zones)
        $(panes t.panes, zones t.zones)
      ::
      ++  pn-abet
        ^+  pa
        =;  =_panes.pag  pa(panes.pag panes)
        =/  panes  panes.pag
        |-
        ?~  panes  [pan]~
        ?:  =(pon nonce.i.panes)
          [pan t.panes]
        [i.panes $(panes t.panes)]
      ::
      ++  pn-nuke
        ^+  pa
        =.  pn
          ?+  +<.pan  pn
            %dill  pn-flee
          ==
        =;  =_panes.pag  pa(panes.pag panes)
        =/  panes  panes.pag
        |-
        ?~  panes  ~
        ?:  =(pon nonce.i.panes)  t.panes
        [i.panes $(panes t.panes)]
      ::
      ::  +|  %details
      ::
      ++  pn-vis
        ::  this pane is visible if the page is visible,
        ::  and it's not solo'd on a different pane.
        ::
        ?&  vis
        ?|  !=(%solo -.style.pag)
            =(pon nonce:pa-fan)
        ==  ==
      ::
      ++  pn-wir
        |=  w=wire
        [%book ses %page pog %pane pon w]
      ::
      ++  pn-sid
        (cat 3 'book--' pon)
      ::
      ::  +|  %emit
      ::
      ++  pn-bell
        pn(bo (bo-blit %bel ~))
      ::
      ::  +|  %process
      ::
      ++  pn-sign  ::  on agent event
        |=  [=wire =sign:agent:gall]
        ^+  pn
        ?.  =(~ wire)  ~|([%book %strange-wire ses pog pon wire] !!)
        ?-  -.sign
          %poke-ack   ?~  p.sign  pn
                      ::TODO  display failure in the ui! it's silent otherwise
                      ((slog 'book poke failed' (flop u.p.sign)) pn)
          %watch-ack  (pn-lack p.sign)
          %kick       pn-look
        ::
            %fact
          ?.  =(%dill-blit p.cage.sign)
            ~&([%book %strange-fact p.cage.sign] pn)
          (pn-blit !<(dill-blit:dill q.cage.sign))
        ==
      ::
      ++  pn-blit
        |=  blit=dill-blit:dill
        ^+  pn
        ?>  ?=(%dill +<.pan)
        ?:  ?=(%qit -.blit)
          !!  ::TODO  handle specially
        ?.  =(~ way.pan)  ::NOTE  tmi
          ?>  ?=(^ way.pan)
          :: =.  que.u.way.pan  [blit que.u.way.pan]
          pn(que.u.way.pan [blit que.u.way.pan])
          :: pn
        =/  di       (di-apex:di zon +.pan)
        =^  red  di  (di-blit:di blit)
        =.  +.pan     di-abet:di
        ?.  pn-vis  pn
        ?+  red  pn
          %&  pn-full ::(biz [[%clr ~] biz])  ::TODO  tmp clr
          ^   pn(bo (bo-blis red))  ::TODO  needs a pn-cure first?
        ==
      ::
      ++  pn-belt  ::  on user event
        |=  belt=dill-belt:dill
        ^+  pn
        ::  transform positional belts to be relative to our zone
        ::
        =?  belt  ?=([%hit *] belt)
          ~|  [%hit-miss belt=belt zone=zon]
          [%hit (sub:co +.belt -.zon)]
        ::
        ?+  +<.pan  pn-bell
            %open
          ::TODO  consider atari code easter egg
          ?:  =(%n belt)
            =>  =/  =gill:gall  [our.bowl %hood]
                pn-look(+.pan [%dill gill ~ *dill-buffer])
            ?>  ?=(%dill +<.pan)
            ::TODO  dedupe with below,
            =.  bo  (bo-dilt (pn-wir /) app.pan pn-sid [%rez +.zon])
            =.  bo  (bo-dilt (pn-wir /) app.pan pn-sid [%yow our.bowl %dojo])
            pn-full
          ?:  =(%p belt)
            pn-full(+.pan [%pick (live [our now]:bowl) [0 0] our.bowl])
          pn-bell
        ::
            %dill
          ?~  way.pan
            =.  bo
              %-  bo-emit
              =/  =cage  [%dill-poke !>([`@tas`pn-sid belt])]
              [%pass (pn-wir /) %agent app.pan %poke cage]
            pn
          ::  scroll mode
          ::
          ?+  belt  pn-bell
            %q         pn-rest
            [%aro %u]  pn-size(pus.u.way.pan (min +(pus.u.way.pan) pn-len))
            [%aro %d]  pn-size(pus.u.way.pan (dec (max 1 pus.u.way.pan)))
          ==
        ::
            %pick
          ::  press return to select & connect
          ::
          ?:  =([%ret ~] belt)
            =>  =/  =gill:gall
                  [who.pan dude:(snag pin.pos.pan cas.pan)]
                pn-look(+.pan [%dill gill ~ *dill-buffer])
            ?>  ?=(%dill +<.pan)
            ::TODO  dedupe with above,
            ::      and also, wtf, how do we know when/what to yow?
            ::TODO  should probably have an easy flow for %hood %yow %dojo,
            ::      and then also a command to be able to send %yow into
            ::      whatever's currently in focus
            =.  bo  (bo-dilt (pn-wir /) app.pan pn-sid [%rez +.zon])
            =.  bo  (bo-dilt (pn-wir /) app.pan pn-sid [%yow our.bowl %dojo])
            pn-full
          ::  shift-r to refresh the list
          ::
          ?:  =('R' belt)
            =;  [c=(list [desk dude:gall]) i=@ud]
              pn-full(cas.pan c, pin.pos.pan i)
            =/  agents  (live [our now]:bowl)
            :-  agents
            %+  fall
              (find [(snag pin.pos.pan cas.pan)]~ agents)
            (min pin.pos.pan (lent agents))
          ::  select by leading char
          ::
          ?@  belt
            ::TODO  implement
            pn-bell
          ::  select using arrow keys
          ::
          ?.  ?=([%aro *] belt)
            pn-bell
          =;  pos=[@ud @ud]
            ?:  =(pos pos.pan)
              pn-bell
            pn-full(pos.pan pos)
          ::NOTE  may crash if cas.pan is empty?
          %+  generic-scroll
            (turn cas.pan |=(* 1))  ::TODO  test with other sizes
          [pin.pos.pan [pew.pos.pan h.zon] p.belt]
        ==
      ::
      ++  pn-roll  ::  enter scroll mode
        ^+  pn
        ?.  ?=(%dill +<.pan)
          pn-bell
        pn-conn(way.pan `[0 ~])
      ::
      ++  pn-rest  ::  exit scroll mode
        ^+  pn
        ~|  [%rest-on-non-scrolling +<.pan]
        ?>  &(?=(%dill +<.pan) !=(~ way.pan))  ::NOTE  tmi
        =/  blis=(list blit)  (flop que:(need way.pan))
        =.  way.pan  ~
        |-
        ?~  blis  pn-size
        ::TODO  dumb stupid assert
        ::TODO  should drop any blits emitted by di-blit,
        ::      since we're redrawing anyway
        =.  pn  =<(?>(?=(%dill +<.pan) .) (pn-blit i.blis))
        $(blis t.blis)
      ::
      ++  pn-size
        ::TODO  somehow doesn't re-render conn when in scroll mode?
        ?+  +<.pan  pn
            %pick
          =-  pn-full(pos.pan (generic-scroll -))
          [(turn cas.pan |=(* 1)) pin.pos.pan [pew.pos.pan h.zon] ~]
        ::
            %dill
          =.  bo  (bo-dilt (pn-wir /) app.pan pn-sid %rez +.zon)
          pn-full(+.pan di-abet:di-melt:(di-apex:di zon +.pan))
        ==
      ::
      ++  pn-look
        ^+  pn
        ?>  ?=(%dill +<.pan)
        =.  ack.pan  ~
        =;  =card  pn(bo (bo-emit card))
        [%pass (pn-wir /) %agent app.pan %watch /dill/[pn-sid]]
      ::
      ++  pn-lack
        |=  err=(unit tang)
        ?>  ?=(%dill +<.pan)
        pn-conn(ack.pan `?=(~ err))
      ::
      ++  pn-flee
        ^+  pn
        ?.  ?=(%dill +<.pan)
          ~&  [%strange-flee +<.pan]
          pn
        ::TODO  +bo-emit? or refactor to have +pn-emit?
        =;  =card  pn(caz [card caz])
        [%pass (pn-wir /) %agent app.pan %leave ~]
      ::
      ::  +|  %display
      ::
      ++  pn-conn  ::TODO  into dill rendering?
        ^+  pn
        ?.  ?=(%dill +<.pan)  pn
        pn(bo (bo-blit re-abet:(re-conn:(re-apex:re zon) pan)))
      ::
      ++  pn-full  =~(pn-body pn-conn)
      ::
      ++  pn-body
        pn(bo (bo-blit re-abet:(re-pane:(re-apex:re zon) pan)))
      ::
      ++  pn-cure
        =;  =blit
          pn(bo (bo-blit blit))
        :: =<  zo-abet
        :: %-  zo-cure:(zo-apex:zo zon)
        %-  ~(cure zi zon)
        ?+  +<.pan  [(dec w.zon) (dec h.zon)]
          %logs  [(dec w.zon) (dec h.zon)]
          %dill  pos.pan
        ::
            %pick
          :-  (dec w.zon)
          ::  draw the cursor at the height a scroll bar might be,
          ::  just to be cute about it
          ::
          =+  l=(lent cas.pan)
          ::  if no scroll needed, stay at top
          ::
          ?:  (lte l h.zon)  0
          ::  otherwise, divide height up in steps,
          ::  and move down as many steps as the viewport has.
          ::
          %-  abs:si  %-  need  %-  ~(toi rs %n)
          %+  mul:rs  (sun:rs pew.pos.pan)
          (div:rs (sun:rs h.zon) (sun:rs (sub l h.zon)))
        ==
      ::
      ++  pn-len  ::  height, for scroll mode
        ?.  ?=(%dill +<.pan)  0
        %+  roll  bac.pan
        |=  [[* lin=(list stub)] s=@ud]
        (add s ?~(lin 1 (lent lin)))
      --
    --
  --
::
::  +di: dill engine
::TODO  could get away with being plain functions in almost all cases?
::
++  di
  |_  [zon=zone con=$>(%dill item)]  ::TODO  just size, not zone?
  +*  di  .
  ::
  ++  di-apex
    |=  [z=zone c=$>(%dill item)]
    di(zon z, con c)
  ::
  ++  di-abet  con
  ::
  ++  di-blit
    |=  bit=blit
    ::  when we can, we take care to preserve or deduce the most-minimal set
    ::  of blits to render the change. this prevents us from needing to redraw
    ::  the entire screen, when a simple %put would do instead.
    ::  this does tie coordinate transformations into this function, but
    ::  the bit of complexity that adds is quite manageable.
    ::
    ^-  [redraw=$@(? (lest blit)) _di]
    ?+  -.bit  [[bit]~ di]
      %clr  [& di(bac.con (weld (reap h.zon [w.zon *(list stub)]) bac.con))]
      %hop  =/  pos=spot  ?@(p.bit [p.bit (dec h.zon)] p.bit)
            [| di(pos.con (min:co pos (sub:co +.zon [1 1])))]
      %put  $(bit [%klr [*stye p.bit]~])
      %klr  (di-draw p.bit)
    ::
        %mor
      =/  red=$@(? (lest blit))  |
      |-
      ~|  (lent p.bit)
      ?~  p.bit  [red di]
      =^  ned=$@(? (lest blit))  di  ^$(bit i.p.bit)
      =?  red  !&(?=(@ red) red)
        ?@  red  ned
        ?^  ned  (welp red ned)
        ?:(ned & red)
      $(p.bit t.p.bit)
    ::
        %nel
      ?.  =(y.pos.con (dec h.zon))
        =.  pos.con  [0 +(y.pos.con)]
        [| di]
      =.  x.pos.con  0
      =.  bac.con    [[w.zon ~] bac.con]  ::TODO  ?
      [& di]
    ::
        %wyp
      :-  [[%hop (add:co [0 y.pos.con] -.zon)] [%put (reap w.zon ~-.)] ~]
      =-  di(bac.con -)
      ::TODO  surely there's _some_ way to generalize logic like this?
      ::      walking down, transforming, etc...
      ::  walk up the buffer until we reach the line to wipe,
      ::  then replace it with the blank line, splitting off any soft-wrapped
      ::  content around it into their own separate lines.
      ::
      ~|  [h=h.zon y=y.pos.con]
      =/  y=@ud  (sub h.zon +(y.pos.con))
      |-
      ?~  bac.con  ~
      ?:  =(~ lin.i.bac.con)
        ?:  =(0 y)  bac.con
        [i.bac.con $(bac.con t.bac.con, y (dec y))]
      =|  but=(list stub)
      |-
      ::TODO  excessive flopping. maybe we want to (lent) instead?
      ?~  lin.i.bac.con
        ::  maintain the lines we just passed
        ::
        [[w.zon (flop but)] ^$(bac.con t.bac.con)]
      ?.  =(0 y)
        ::  store a visual line
        ::
        $(but [i.lin.i.bac.con but], lin.i.bac.con t.lin.i.bac.con, y (dec y))
      ::  place an empty line and conclude with the remainder, if any
      ::
      =;  bup=(list [@ud (list stub)])
        ?~  but  bup
        [[w.zon (flop but)] bup]
      :-  [w.zon ~]
      ?~  t.lin.i.bac.con  t.bac.con
      [[w.zon t.lin.i.bac.con] t.bac.con]
    ==
  ::
  ++  di-draw
    |=  s=stub
    ^-  [$@(%& (lest blit)) _di]
    ::  the below code is quite verbose, handling every possible case
    ::  explicitly. it was difficult to get exactly right, but the verbosity
    ::  helped. there might be branches that never get hit in practice.
    ::  it seems performant enough, doing the smallest possible buffer
    ::  traversal.
    ::
    ::  broadly, the logic below is divided into three phases:
    ::  0) wrap the stub at the appropriate width and figure out the
    ::     minimal diff to render it onto the screen.
    ::  1) walk up the buffer, grabbing semantic lines at and below the
    ::     point where we'll be drawing.
    ::  2) walk back down the buffer, merging in the newly drawn content.
    ::     here we take care to merge any semantic lines that get connected
    ::     by the new content, for the most realistic terminal experience.
    ::
    ::  phase 0: pre-draw, splitting into visual lines,
    ::
    ~?  (gth x.pos.con w.zon)
      [%uh-ooh-underflow zon=zon pos=pos.con]
    =/  [hed=stub tal=stub]  (trim:klr (sub w.zon x.pos.con) s)
    =/  tal=(list stub)      (flow:klr w.zon tal)
    ::
    :-  ?:  (gte (lent tal) (sub h.zon y.pos.con))
         ::  we overflow, scrolling other content up,
         ::  so we must do a full redraw
         ::
          %&
        ::  we don't overflow, so can simply draw on top
        ::
        =/  pon=spot  (add:co pos.con -.zon)
        :+  [%hop pon]
          [%klr hed]
        =.  x.pon  x.zon
        |-
        ?~  tal  ~
        =.  y.pon  +(y.pon)
        [[%hop pon] [%klr i.tal] $(tal t.tal)]
    ::
    =;  =_bac.con  di(bac.con bac)
    ::
    ::  phase 1: walk up, accumulating for the way down
    ::
    ~?  (gth y.pos.con h.zon)
      [%uh-ohh-underflow zon=zon pos=pos.con]
    =/  i=@ud  (sub h.zon y.pos.con)
    =|  pas=@ud
    =|  bot=(list (list stub))  ::  top-first
    |-  ^+  bac.con
    ?.  =(0 i)
      ::  if we're past the end of the buffer, store empty lines
      ::
      ?~  bac.con
        $(i (dec i), bot [~ bot])
      ::  store empty buffer lines as-is
      ::
      ?:  =(~ lin.i.bac.con)
        $(i (dec i), bot [~ bot], bac.con t.bac.con)
      ::  for non-empty lines, make sure we grab the whole semantic line,
      ::  and record how far we overshoot
      ::
      =|  bin=(list stub)
      |-
      ?~  lin.i.bac.con
        ^$(bot [bin bot], bac.con t.bac.con)
      =?  pas  =(0 i)  +(pas)
      =?  i    =(0 i)  +(i)
      $(i (dec i), bin [i.lin.i.bac.con bin], lin.i.bac.con t.lin.i.bac.con)
    ::
    ::  phase 2: walk down, drawing where appropriate
    ::
    =;  out=(list (list stub))
      %+  weld
        (turn (flop out) (lead w.zon))
      bac.con
    =/  g=?  |
    =|  but=(list stub)
    |-  ^-  (list (list stub))
    ?~  bot
      ::  we're at the end of the buffer, flush the remainder
      ::
      ?>  g
      ?~  but
        ?~  tal  ~
        [tal]~
      |-
      ?~  tal  [but]~
      $(but [i.tal but], tal t.tal)
    ::  if we're dealing with an empty line
    ::
    ?:  =(~ i.bot)
      ::  if we are before the draw point
      ::
      ?.  =(0 pas)  ::NOTE  in practice this case can't happen right?
        ::  we are not yet drawing, put the empty line and continue down
        ::
        [~ $(pas (dec pas), bot t.bot)]
      ::  and this is the first time drawing
      ::
      ?.  g
        ::  pad the first line
        ::
        =.  hed  [[*stye (reap x.pos.con ~-.)] hed]
        ?~  tal
          ::  no further lines will follow, so we draw right away
          ::
          ~?  !=(~ but)  [%book %strange-non-empty-bot]
          [[hed but] $(but ~, bot t.bot, g &)]
        ::  further lines will follow, so we store for now
        ::
        $(but [hed but], bot t.bot, g &)
      ::  store if there's still tal remaining
      ::
      ?^  tal
        ?~  t.tal
          ::  if this is last tal, draw right away, since we're at line bound
          ::
          [[i.tal but] $(but ~, tal t.tal, bot t.bot)]
        ::  otherwise, store, continue
        ::
        $(but [i.tal but], tal t.tal, bot t.bot)
      ::  otherwise just blank line
      ::
      ?~  but
        [~ $(bot t.bot)]
      [but ~ $(bot t.bot, but ~)]
    ::  iterate over semantic lines
    ::
    |-
    ::  if we are before the draw point
    ::
    ?.  =(0 pas)
      ?~  i.bot
        ::  end of semantic line, draw and go to next
        ::
        ~?  =(~ but)  %strange-empty-but
        [but ^$(bot t.bot, but ~)]
      ::  still visual lines remaining, store them & move on
      ::
      $(pas (dec pas), but [i.i.bot but], i.bot t.i.bot)
    ::  if this is the first time drawing
    ::
    ?.  g
      ?~  i.bot  [but ^$(bot t.bot, but ~)]
      ::  wail the first line, continue
      ::
      =-  $(but [- but], i.bot t.i.bot, g &)
      ::TODO  should this +pact:klr?
      (wail:klr i.i.bot x.pos.con hed ~-.)
    ::  if there's still tal remaining, store it
    ::
    ?^  tal
      ?~  i.bot  ^$(bot t.bot)
      ::  if this is the last tal, wail it
      ::
      ?~  t.tal
        =-  $(but [- but], tal t.tal, i.bot t.i.bot)
        ::TODO  should this +pact:klr?
        (wail:klr i.i.bot 0 i.tal ~-.)
      ::  otherwise, simply put the full line
      ::
      $(but [i.tal but], tal t.tal, i.bot t.i.bot)
    ::  no tal remaining, simply restore bot
    ::
    ?~  i.bot  [but ^$(bot t.bot, but ~)]
    $(but [i.i.bot but], i.bot t.i.bot)
  ::
  ++  di-melt  ::  called on-blew and on-scroll
    ^+  di
    ~>  %bout.[0 'book: di-melt']
    ::  when a resize happens, our previous soft-wrapping of the scrollback
    ::  lines becomes invalidated. re-wrap the lines on-screen.
    ::
    =-  di(bac.con -)
    ::  note that we need to flop, because we want bottom-first data
    ::NOTE  performance on the reset function might be avoidable?
    ::
    %:  generic-melt
      w.zon
      [?~(way.con 0 pus.u.way.con) `h.zon]
      [|=(l=(list stub) (pact:klr (zing (flop l)))) trim:klr flop]
      bac.con
    ==
  --
::
::  +re: rendering engine
::
++  re
  =|  biz=(list blit)
  |_  =zone
  +*  re  .
  ::
  ++  re-apex
    |=  z=^zone
    re(zone z)
  ::
  ++  re-blit
    |=  b=blit
    re(biz [b biz])
  ::
  ++  re-abet
    ^-  blit:dill
    ?:  ?=([* ~] biz)  i.biz
    [%mor (flop biz)]
  ::
  ++  re-pane
    |=  =pane
    |^  ?-  +<.pane
          %open  re-pane-open
          %logs  re-pane-logs
          %dill  re-pane-dill
          %pick  re-pane-pick
        ==
    ::
    ++  re-pane-open
      ?>  ?=(%open +<.pane)
      ?.  &((gte w.zone 65) (gte h.zone 19))
        ::  tiny usage instructions
        ::
        =>  |%
            ++  title
              [[(sy %br %un ~) ~ ~] `(list @)`"%book: dill multiplexer"]~
            ++  drum  ^~  %-  make:klr
              ~['press ' [`%un ~ ~]^"n" ' to open a new drum session.']
            ++  pick  ^~  %-  make:klr
              ~['      ' [`%un ~ ~]^"p" ' to pick any other agent.']
            ++  side
              |=([=spot =stub] (~(stub zi zone) spot (side:klr %c w.zone stub)))
            --
        =/  zer  ~(. zi zone)
        =.  re  (re-blit wipe:zer)
        =.  re  (re-blit (stub:zer [0 0] title))
        =.  re  (re-blit (stub:zer [0 1] drum))
        =.  re  (re-blit (stub:zer [0 2] pick))
        re
      ::NOTE  when changing :hed's dimensions, reconsider h/w reqs,
      ::      the below +splits and +center calls depend on it
      =.  re  (re-blit ~(wipe zi zone))
      =/  hed=wain
        :~  ' ▗▄▄▄▄·   ▪   ▪   . ▄ •▄   '
            '  ▐█ ▀█▪ ▄█▀▄  ▄█▀▄ █▌▄▌▪  '
            '. ▐█▀▀▙▄▐█▌.▐▌▐█▌ ▐▌▐▀▀▄·  '
            '  ██▄▪▐█ ▀█▄▀  ▀█▄▀▪▐█.█▌  '
            '  ·▀▀▀▀     ·       ·▀  ▀ ·'
            '~  the dill multiplexer   ~'
        ==
      ::TODO  the usage of +splits and +join here hints at how a more powerful
      ::      +splits might be of use to us
      =/  zas  (splits:za zone [%h ~[~ ~ ~]])
      =/  log  (snag 0 zas)
      =/  key  (join:za (snag 1 zas) (snag 2 zas))
      ::
      =/  pos  (center:za +.log ~(size wa hed))
      =.  re  (re-blit (~(wain zi log) pos hed))
      ::
      ::NOTE  careful, the stubs below shouldn't be too wide/tall!
      =/  kez=(list stub)  ^~
        =;  stys=(list styx)
          %+  turn  (turn stys make:klr)
          (cury (cury side:klr %c) w.key)
        :~  ~['press ' [`%un ~ ~]^"n" ' to open a new drum session,']
            ~['      ' [`%un ~ ~]^"p" ' to pick any other agent.   ']
            ~
            :~  'the prefix is '
                =;  =tape  [`%br ~ ~]^tape
                =/  pre=belt:dill  prefix:keys:config
                |-  ^-  tape
                ?@  pre
                  ?+  pre  `(list @)`[pre]~  ::TODO  safety
                    %' '  "space"
                    :: %''  ::TODO  tab
                  ==
                ?:  ?=(%txt -.pre)  `(list @)`p.pre  ::TODO  safety
                ?:  ?=(%mod -.pre)
                  (weld (trip mod.pre) '+' $(pre key.pre))
                ?-  -.pre
                  %aro  ?-(p.pre %r "right", %l "left", %u "up", %d "down")
                  %bac  "backspace"
                  %del  "delete"
                  %hit  "click at ({(scow %ud x.pre)},{(scow %ud y.pre)})"
                  %ret  "return"
                ==
            ==
            ~
            :~  [`%br ~ ~]^"<prefix> C"  '  create new page    '
                '   '
                [`%br ~ ~]^"<prefix> c"  '  create new pane    '
            ==
            :~  [`%br ~ ~]^"<prefix> X"  '  delete current page'
                '   '
                [`%br ~ ~]^"<prefix> x"  '  delete current pane'
            ==
            :~  [`%br ~ ~]^"<prefix> 0"  '  go to page 0       '
                '   '
                [`%br ~ ~]^"<prefix> s"  '  switch layout      '
            ==
            :~  [`%br ~ ~]^"<prefix> ,"  '  rename page        '
                '   '
                [`%br ~ ~]^"<prefix> z"  '  focus mode         '
            ==
          ==
      =/  y=@ud  0
      |-
      ?~  kez  re
      =.  re  (re-blit (~(stub zi key) [0 y] i.kez))
      $(kez t.kez, y +(y))
      :: =.  re  %-  re-blit
      ::   %+  side  [0 (add mid 4)]
      ::   ^~  %-  make:klr
      ::   :~  [`%br ~ ~]^"<prefix> c"
      ::       '  create new page    '
      ::       [`%br ~ ~]^"<prefix> x"
      ::       '  delete current page'
      ::   ==
      :: =.  re  %-  re-blit
      ::   %+  side  [0 (add mid 5)]
      ::   ^~  %-  make:klr
      ::   :~  [`%br ~ ~]^"<prefix> c"
      ::       '  create new pane    '
      ::       [`%br ~ ~]^"<prefix> x"
      ::       '  delete current page'
      ::   ==
      :: =.  re  %-  re-blit
      ::   %+  side  [0 (add mid 6)]
      ::   ^~  %-  make:klr
      ::   :~  [`%br ~ ~]^"<prefix> c"
      ::       '  create new page    '
      ::       [`%br ~ ~]^"<prefix> x"
      ::       '  delete current page'
      ::   ==
      ::
      :: re
    ::
    ++  re-pane-open-old
      ?>  ?=(%open +<.pane)
      =.  re
        =/  wyp=stub  [*stye (reap w.zone ~-.)]~
        =/  y=@ud  0
        |-
        ?:  =(y h.zone)  re
        =.  re  (re-blit (~(stun zi zone) [0 y] wyp))
        $(y +(y))
      =>  |%
          ++  title
            [[(sy %br %un ~) ~ ~] `(list @)`"%book: dill multiplexer"]~
          ++  drum  ^~  %-  make:klr
            ~['press ' [`%un ~ ~]^"n" ' to open a new drum session.']
          ++  pick  ^~  %-  make:klr
            ~['press ' [`%un ~ ~]^"p" ' to choose any other agent.']
          ++  side
            |=([=spot =stub] (~(stub zi zone) spot (side:klr %c w.zone stub)))
          --
      ?.  &((gte w.zone 64) (gte h.zone 12))
        ::  tiny usage instructions
        ::
        =.  re  (re-blit (~(stub zi zone) [0 0] title))
        =.  re  (re-blit (~(stub zi zone) [0 1] drum))
        =.  re  (re-blit (~(stub zi zone) [0 2] pick))
        re
      ::  larger splash screen & usage instructions
      ::TODO  for even larger, render pixel art "%book" test
      ::    __
      :: 0/|__) _  _ |
      :: /0|__)(_)(_)|(
      ::
      :: ▄▄▄▄·             ▄ •▄
      :: ▐█ ▀█▪▪     ▪     █▌▄▌▪
      :: ▐█▀▀█▄ ▄█▀▄  ▄█▀▄ ▐▀▀▄·
      :: ██▄▪▐█▐█▌.▐▌▐█▌.▐▌▐█.█▌
      :: ·▀▀▀▀  ▀█▄▀▪ ▀█▄▀▪·▀  ▀
      ::
      :: ▀█████████▄   ▄██████▄   ▄██████▄     ▄█   ▄█▄
      ::   ███    ███ ███    ███ ███    ███   ███ ▄███▀
      ::   ███    ███ ███    ███ ███    ███   ███▐██▀
      ::  ▄███▄▄▄██▀  ███    ███ ███    ███  ▄█████▀
      :: ▀▀███▀▀▀██▄  ███    ███ ███    ███ ▀▀█████▄
      ::   ███    ██▄ ███    ███ ███    ███   ███▐██▄
      ::   ███    ███ ███    ███ ███    ███   ███ ▀███▄
      :: ▄█████████▀   ▀██████▀   ▀██████▀    ███   ▀█▀
      ::                                      ▀
      ::
      ::
      =/  mid=@ud  (div h.zone 2)
      =/  top=@ud  (div +(mid) 2)
      ::
      =.  re  %-  re-blit  %+  side  [0 top]        title
      =.  re  %-  re-blit  %+  side  [0 (dec mid)]  drum
      =.  re  %-  re-blit  %+  side  [0 mid]        pick
      ::
      =.  re  %-  re-blit
        %+  side  [0 (add mid 2)]
        ^~  %-  make:klr
        ~['the prefix is ' [`%br ~ ~]^"ctl+x" '.']
      =.  re  %-  re-blit
        %+  side  [0 (add mid 4)]
        ^~  %-  make:klr
        :~  [`%br ~ ~]^"<prefix> c"
            '  create new page    '
            [`%br ~ ~]^"<prefix> x"
            '  delete current page'
        ==
      =.  re  %-  re-blit
        %+  side  [0 (add mid 5)]
        ^~  %-  make:klr
        :~  [`%br ~ ~]^"<prefix> c"
            '  create new pane    '
            [`%br ~ ~]^"<prefix> x"
            '  delete current page'
        ==
      =.  re  %-  re-blit
        %+  side  [0 (add mid 6)]
        ^~  %-  make:klr
        :~  [`%br ~ ~]^"<prefix> c"
            '  create new page    '
            [`%br ~ ~]^"<prefix> x"
            '  delete current page'
        ==
      re
    ::
    ++  re-pane-logs
      ?>  ?=(%logs +<.pane)
      =/  y=@ud  h.zone
      |^  ?:  =(0 y)  re
          =.  y  (dec y)
          ?~  log.pane
            $(re (line ~))
          =.  re  ::TODO  this sucks
            :: =<  ?>(?=(%logs +<.pane) ?>(?=(^ log.pane) .))
            (line i.log.pane)
          $(log.pane t.log.pane)
      ::
      ++  line
        |=  s=stub
        %-  re-blit
        %+  ~(stun zi zone)  [0 y]
        ::TODO  wrap instead of crop
        ::NOTE  performance
        ::TODO  test w/ bg color
        :: (scag:klr w.zone (wail:klr s w.zone ~ ~-.))
        %+  wail:klr
          [[~ ?:(=(0 y.zone) %r ?:((gth y.zone 15) %b ~)) ~] (reap w.zone ~-.)]~
        [0 s ~-.]
      --
    ::
    ::TODO  slightly weird that this in not in +di or w/e
    ++  re-pane-dill
      ?>  ?=(%dill +<.pane)
      =/  y=@ud  h.zone
      =/  d=@ud  ?~(way.pane 0 pus.u.way.pane)
      ::TODO  something about this is borked...
      :: =?  bac.pane  !=(0 d)
      ::   (generic-scag d bac.pane)
      |-
      ?.  =(0 d)
        ::TODO  can we substitute with +generic-scag ?
        ?~  bac.pane  $(d 0)
        ?:  =(~ lin.i.bac.pane)
          $(d (dec d), bac.pane t.bac.pane)
        |-
        ?:  =(0 d)  ^$
        ?~  lin.i.bac.pane  ^$(bac.pane t.bac.pane)
        ?~  t.lin.i.bac.pane
          ^$(d (dec d), bac.pane t.bac.pane)
        $(d (dec d), lin.i.bac.pane t.lin.i.bac.pane)
      |^  ?:  =(0 y)  re
          ::TODO  slightly awkward factoring here
          =.  y  (dec y)
          ?~  bac.pane
            $(re (line ~))
          ~?  !=(w.zone wid.i.bac.pane)
            [%unprepared-dill-backlog want=w.zone have=wid.i.bac.pane]
          =?  lin.i.bac.pane  =(%debug-rendering-bounds %no)
            %-  flop
            =/  lin  (flop lin.i.bac.pane)
            =/  first=?  &
            |-  ^-  (list stub)
            ?~  lin  ~
            =?  i.lin  first
              (wail:klr i.lin 0 [[~ %r ~] `(list @)`"["]~ ~-.)
            =.  first  |
            ?^  t.lin
              [i.lin $(lin t.lin)]
            :_  ~
            %:  wail:klr
              i.lin
              (dec (lent-char:klr i.lin))
              [[~ %b ~] `(list @)`"]"]~
              ~-.
            ==
          =?  lin.i.bac.pane  =(%debug-rendering-widths %no)
            %+  turn  lin.i.bac.pane
            |=  =stub
            (wail:klr stub 0 [[~ %r ~] `(list @)`(scow %ud wid.i.bac.pane)]~ ~-.)
          ?~  lin.i.bac.pane
            $(re (line ~), bac.pane t.bac.pane)
          |-
          =.  re  (line i.lin.i.bac.pane)
          ?~  t.lin.i.bac.pane
            ^$(bac.pane t.bac.pane)
          ?:  =(0 y)  re
          $(lin.i.bac.pane t.lin.i.bac.pane, y (dec y))
      ::
      ++  line
        |=  s=stub
        %-  re-blit
        %+  ~(stun zi zone)  [0 y]
        ::NOTE  performance
        (wail:klr [*stye (reap w.zone ~-.)]~ 0 s ~-.)
      --
    ::
    ++  re-pane-pick
      ::TODO  refactor into generic list selection logic
      ?>  ?=(%pick +<.pane)
      ::  l: total items
      ::  r: visual row of selected item
      ::
      =/  l=@ud     (lent cas.pane)
      =/  r=@ud     (sub [pin pew]:pos.pane)
      =.  cas.pane  (slag pew.pos.pane cas.pane)
      =/  y=@ud     0
      |-
      ?:  (gte y h.zone)  re
      ?~  cas.pane
        =.  re
          %-  re-blit
          (~(stub zi zone) [0 y] [*stye (reap w.zone ~-.)]~)
        $(y +(y))
      =*  line  i.cas.pane
      =.  re
        %-  re-blit
        %+  ~(stub zi zone)  [0 y]
        =/  lin=(list @)
          "%{(trip desk.line)}/{(trip dude.line)}"
        =?  lin  (lth (lent lin) w.zone)
          (weld lin (reap (sub w.zone (lent lin)) ' '))
        =/  sty=stye
          [~ ?:(=(r y) [%k %w] [~ ~])]
        [sty lin]~
      $(y +(y), cas.pane t.cas.pane)
    --
  ::
  ++  re-conn
    |=  =pane ::=$>(%dill pane)
    ?>  ?=(%dill +<.pane)
    ^+  re
    =/  top=stub
      ::TODO  probably want some generic "read part of buffer" function
      ::  walk up the buffer until we reach the top line, grab it
      ::
      ::TODO  partially copied from +re-pane-dill. refactor!
      =/  d=@ud  ?~(way.pane 0 pus.u.way.pane)
      |-
      ?.  =(0 d)
        ?~  bac.pane  $(d 0)
        ?:  =(~ lin.i.bac.pane)
          $(d (dec d), bac.pane t.bac.pane)
        |-
        ?:  =(0 d)  ^$
        ?~  lin.i.bac.pane  ^$(bac.pane t.bac.pane)
        ?~  t.lin.i.bac.pane
          ^$(d (dec d), bac.pane t.bac.pane)
        $(d (dec d), lin.i.bac.pane t.lin.i.bac.pane)
      |-
      ?~  bac.pane  ~
      ?:  =(1 h.zone)
        ?~(lin.i.bac.pane ~ i.lin.i.bac.pane)
      ?:  =(~ lin.i.bac.pane)
        $(h.zone (dec h.zone), bac.pane t.bac.pane)
      |-
      ?~  lin.i.bac.pane  ^$(bac.pane t.bac.pane)
      ?:  =(1 h.zone)  i.lin.i.bac.pane
      $(h.zone (dec h.zone), lin.i.bac.pane t.lin.i.bac.pane)
    =;  [=stye text=(list @)]
      %-  re-blit
      %+  ~(stun zi zone)  [0 0]
      ::TODO  underflow risk!
      (wail:klr top (sub w.zone (lent text)) [stye text]~ ~-.)
    ?~  way.pane
      ?-  ack.pane
        [~ %&]  [*stye ~]
        [~ %|]  [[~ %r %w] "disconnected"]
        ~       [[~ ~ %b] "connecting.."]
      ==
    :-  [~ %y %k]
    =;  max=@ud
      "[{(scow %ud pus.u.way.pane)}/{(scow %ud max)}]"
    ::TODO  deduplicate with +pn-len
    %+  roll  bac.pane
    |=  [[* lin=(list stub)] s=@ud]
    (add s ?~(lin 1 (lent lin)))
  --
::
++  klr
  ::TODO  could probably rewrite klr:format to be faster? +scag is weird at least
  =,  klr:format
  |%
  ++  trim  ::  split
    ::  notably 40% faster than the naive [(scag a b) (slag a b)]
    ::NOTE  you'd think +wail could use this, but it's not much faster?
    |=  [a=@ud b=stub]
    ^-  (pair stub stub)
    ?~  b  [~ ~]
    =/  l  (lent q.i.b)
    ?:  =(a l)  [[i.b]~ t.b]
    ?:  (lth a l)
      :-  [[p.i.b (^scag a q.i.b)] ~]
      [[p.i.b (^slag a q.i.b)] t.b]
    ::NOTE  ?>  (gth a l)
    ::  uh oh, stack alert!
    =/  r  $(a (sub a l), b t.b)
    [[i.b p.r] q.r]
  ::
  ::TODO  we practically always do (flop (flow ...)), because we want
  ::      bottom-first scrollback logs. this doesn't belong in +klr's context...
  ++  flow  ::  hard-wrap w/o boundaries
    |=  [a=@ud b=stub]
    |-  ^-  (list stub)
    ?:  =(~ b)  ~
    =^  c=stub  b  (trim a b)
    ?:  =(~ b)  [c ~]
    [c $]
  ::
  ++  side  ::  pad & align
    |=  $:  a=?(%l %c %r)
            b=$@(@ud [w=@ud s=stye c=@c])
            c=stub
        ==
    ^-  stub
    =?  b  ?=(@ b)  [b *stye ~-.]
    ?>  ?=(^ b)
    ::NOTE  performance?
    =/  d=@ud  (lent-char c)
    ?:  (gte d w.b)  c
    =/  e=@ud  (sub w.b d)
    ?-  a
      %l  (snoc c [s.b (reap e c.b)])
      %r  [[s.b (reap e c.b)] c]
      %c  =/  r=@ud  (div e 2)
          =/  l=@ud  (sub e r)
          (snoc [[s.b (reap l c.b)] c] [s.b (reap r c.b)])
    ==
  --
::
++  prod
  |*  [a=(list) b=@ud c=gate]
  ?~  a  !!
  ?:  =(0 b)  [(c i.a) t.a]
  [i.a $(a t.a, b (dec b))]
::
++  simp
  |=  s=stub
  ;;((list tape) (turn s tail))  ::TODO  +tufa
::
++  live
  |=  [our=@p now=@da]
  =/  desks=(list desk)
    =-  (sort ~(tap in -) aor)
    .^((set desk) %cd /(scot %p our)//(scot %da now))
  |-
  ?~  desks  ~
  =/  nodes=(list dude:gall)
    =-  (sort - aor)
    %+  murn
      %~  tap  in
      .^  (set [dude:gall ?])  %ge
        /(scot %p our)/[i.desks]/(scot %da now)
      ==
    |=([d=dude:gall l=?] ?.(l ~ (some d)))
  |-
  ?~  nodes  ^$(desks t.desks)
  [[i.desks i.nodes] $(nodes t.nodes)]
--
