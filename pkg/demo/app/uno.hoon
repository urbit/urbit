::  uno: ultra-minimal text editor in the style of nano
::
::TODO
::  want to make this a good basis for forking and making your own editor.
::  ...but what does that take? just havng primitives for all the different
::  editor styles? (modes, (prefix) shortcuts, etc.) that might not be
::  sufficient for more "radical" kinds of editors, can we help them out
::  beyond text buffer rendering?
::
/+  etui, cyst, dbug, verb, default-agent
::
|%
+$  state-0
  $:  %0
      sessions=(map @ta session)
  ==
::
+$  session
  $:  size=[w=@ud h=@ud]
      info=(unit @t)
      $=  what
  $%  [%picker picker]
      [%buffer buffer]
  ==  ==
::
+$  picker
  :: $:  inp=[buf=@t cur=@ud]
  :: ==
  $:  des=(list node)
      cur=@ud
      off=@ud
  ==
::
+$  buffer
  $:  ::  fro: case the file was originally loaded from, or last saved to,
      ::       so that we may warn about overwriting changes that happened
      ::       in the background.
      ::  pos: line-column cursor position, w/ desired column
      ::TODO  if visual line-column, how does the desired column work wrt wrap?
      ::  buf: soft-wrapped contents
      bat=bath
      fro=(unit [cas=@ud mug=@uw])
      vew=[x=@ud y=@ud]  ::NOTE  soft-wrapped buffer wouldn't need x: vew=@ud
      pos=[[c=@ud l=@ud] d=@ud]
      act=$~(~ ?(~ [%find t=@t] [%line t=@t]))
      fin=@t
      buf=wain  ::  (list @t)
      ::NOTE  soft-wrapped buffer: buf=(list [wid=@ud lin=(list @t)])
  ==
::
::TODO  if only one child, render as multi-node path
::      could store (map path ~) instead of (map @ta ~)
::TODO  also need to support creating new files...
:: +$  node  [nom=@ta fil=? vew=? dir=(list node)]
:: +$  node  $@(@ta [nom=@ta vew=? dir=(lest node)])
+$  node  ::  pre-flattened
  $~  [%file /]
  $%  [%file pax=path]
      [%tree pax=path vew=? dir=(list node)]
  ==
+$  bath  [=desk =spur]
::
++  cy
  |^  (cyst open save)
  ++  open
    |=  n=node
    ^-  (list node)
    ?.  ?=(%tree -.n)  ~
    ?.  vew.n  ~
    dir.n
  ::
  ++  save
    |=  [n=node d=(list node)]
    ^-  node
    ?>  ?=(%tree -.n)
    n(dir d)
  --
::
+$  card  card:agent:gall
+$  blit  blit:dill
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
    =/  sez=(list @ta)  ~(tap in ~(key by sessions))
    =|  caz=(list card)
    |-
    ?~  sez  [caz this]
    =^  cas  sessions  se-abet:(se-belt:(se-apex:se i.sez bowl) [%hey ~])
    $(caz (weld caz cas), sez t.sez)
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?.  ?=(%dill-poke mark)  (on-poke:def mark vase)
    =+  !<([ses=@ta belt=dill-belt:dill] vase)
    ::TODO  should dill send %rez before %hey?
    =^  caz  sessions  se-abet:(se-belt:(se-apex:se ses bowl) belt)
    [caz this]
  ::
  ++  on-watch
    |=  =path
    ?.  ?=([%dill @ ~] path)  (on-watch:def path)
    [~ this]  :: [~ this(sessions (~(put in sessions) i.t.path))]
  ::
  ++  on-leave
    |=  =path
    ?.  ?=([%dill @ ~] path)  (on-leave:def path)
    [~ this]  :: [~ this(sessions (~(del in sessions) i.t.path))]
  ::
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-peek   on-peek:def
  ++  on-fail   on-fail:def
  --
::
|%
::  +se: session engine
::
++  se
  =|  biz=(list blit)
  =|  caz=(list card)
  |_  [sid=@ta ses=session =bowl:gall]
  +*  se  .
  ::
  ++  se-apex
    |=  [s=@ta b=bowl:gall]
    ^+  se
    =.  bowl  b
    =.  sid  s
    =.  se
      ?:  (~(has by sessions) sid)
        se(ses (~(got by sessions) sid))
      se-pick(ses [[80 24] ~ [%picker ~ 0 0]])
    ?:  (~(has by sessions) sid)  se
    se-draw
    :: %+  turn
    ::   =-  (sort - aor)
    ::   %~  tap  in
    ::   .^((set desk) %cd /(scot %p our.bowl)//(scot %da now.bowl))
    :: |=  [dek=desk]
    :: :+  dek  |
    :: .^((list path) %ct /(scot %p our.bowl)/[dek]/(scot %da now.bowl))
  ::
  ++  se-pick
    ^+  se
    =;  dez=(list node)
      se(what.ses [%picker dez 0 0])
    %+  murn
      (sort ~(tap in .^((set desk) %cd /(scot %p our.bowl)//0)) aor)
    |=  d=desk
    ^-  (unit node)
    `[%tree /[d] | ~]
    :: =;  nos=(list node)
    ::   ?~  nos  ~
    ::   %-  some
    ::   ?^  t.nos  [%tree /[d] | nos]
    ::   ?-  -.i.nos
    ::     %file  [%file d pax.i.nos]
    ::     %tree  i.nos(pax [d pax.i.nos])
    ::   ==
    :: =/  pax=path  /
    :: =/  =path  /(scot %p our.bowl)/[d]/(scot %da now.bowl)
    :: ::TODO  only enumerate on-fold-open
    :: |-  ^-  (list node)
    :: =*  next-arch  $
    :: ~&  [%cy path]
    :: =/  arch  .^(arch %cy path)
    :: |-  ^-  (list node)
    :: =*  this-arch  $
    :: ?^  fil.arch
    ::   :-  [%file pax]
    ::   this-arch(fil.arch ~)
    :: =/  dirs  (sort ~(tap in ~(key by dir.arch)) aor)
    :: |-  ^-  (list node)
    :: =*  this-dirs  $
    :: ?~  dirs  ~
    :: ?~  t.dirs
    ::   next-arch(pax (snoc pax i.dirs), path (snoc path i.dirs))
    :: :_  ~ ::next-arch(dirs t.dirs)
    :: :^  %tree  pax  &
    :: =;  l=(list node)  ?>(?=(^ l) l)
    :: %-  zing
    :: %+  turn  dirs
    :: |=  d=@ta
    :: next-arch(pax /[d], path (snoc path d))
  ::
  ++  se-abet
    ^-  (quip card _sessions)
    :_  (~(put by sessions) sid ses)
    ?~  biz  (flop caz)
    =;  bit=card
      [bit (flop caz)]
    [%give %fact [/dill/[sid]]~ %dill-blit !>(`blit`[%mor (flop biz)])]
  ::
  ++  se-belt
    |=  belt=dill-belt:dill
    ^+  se
    ?+  belt
      ?-  -.what.ses
        %picker  pi-abet:(pi-belt:se-pic belt)
        %buffer  bu-abet:(bu-belt:se-buf belt)
      ==
    ::
      [%hey *]  se-draw
      [%rez *]  (se-size +.belt)
      ?([%yow *] [%cru *])  se
    ==
  ::
  ++  se-size
    |=  s=_size.ses
    ^+  se
    ~&  [%se-size s]
    ?:  =(s size.ses)  se
    =.  size.ses  s
    ?-  -.what.ses
      %picker  pi-abet:pi-draw:se-pic  ::TODO
      %buffer  bu-abet:bu-size:se-buf
    ==
  ::
  ++  se-draw
    ^+  se
    ?-  -.what.ses
      %picker  pi-abet:pi-draw:se-pic
      %buffer  bu-abet:bu-draw:se-buf
    ==
  ::
  ++  se-load
    |=  =bath
    ^+  se
    bu-abet:bu-load:se-buf(what.ses [%buffer %*(. *buffer bat bath)])
  ::
  ++  se-save
    ^+  se
    se  ::TODO
  ::
  ++  se-pic  ?>(?=(%picker -.what.ses) (pi-apex:pi +.what.ses))
  ++  se-buf  ?>(?=(%buffer -.what.ses) (bu-apex:bu +.what.ses))
  ::  +pi: filepicker engine
  ::
  ++  pi
    |_  [pic=picker bat=(unit path)]
    +*  pi  .
    ::
    ++  pi-apex
      |=  p=picker
      pi(pic p)
    ::
    ++  pi-abet
      ^+  se
      ?^  bat  (se-load ?>(?=(^ u.bat) u.bat))
      se(what.ses [%picker pic])
    ::
    ++  pi-belt
      |=  belt=dill-belt:dill
      ^+  pi
      ?:  ?=([%hit *] belt)
        pi
        :: =-  pi(biz [[%mor -] biz])
        :: ^-  (list blit)
        :: :~  [%hop +.belt]
        ::     [%wyp ~]
        :: ==
      ?:  ?=([%rez *] belt)
        pi  ::TODO?
      ?:  ?=([%ret *] belt)
        =+  pi-path
        ?.  fil  pi(biz [[%bel ~] biz])
        pi(bat `pax)
      ?:  ?=([%hey ~] belt)
        pi-draw
      ?:  ?=(?([%yow *] [%cru *]) belt)
        pi
      ?:  ?=([%aro %d] belt)
        ~&  lent+(lent:cy des.pic)
        pi-draw:pi-view(cur.pic (min +(cur.pic) (dec (lent:cy des.pic))))
      ?:  ?=([%aro %u] belt)
        ?:  =(0 cur.pic)  pi
        pi-draw:pi-view(cur.pic (dec cur.pic))
      ?:  ?=([%aro ?(%l %r)] belt)
        pi-draw:(pi-fold ?=(%r p.belt))
      pi
      :: =/  new=[@t @ud]  (generic-input:etui inp.pic belt)
      :: ?:  =(new inp.pic)
      ::   pi(biz [[%bel ~] biz])
      :: pi-draw(inp.pic new)
    ::
    ++  pi-view
      ::TODO  update off.pic to ensure cur.pic is in view
      =<  ~&  [off=off.pic cur=cur.pic len=(lent:cy des.pic)]  .
      =-  pi(off.pic -)
      ?:  (lth cur.pic off.pic)  cur.pic
      ?:  (gte cur.pic (add off.pic (dec h.size.ses)))
        (sub cur.pic (dec h.size.ses))
      off.pic
    ::
    ++  pi-fold
      |=  vew=?
      =-  ?.  =(- des.pic)
            pi(des.pic -)
          ::TODO  move cur.pic to folded-open dir directly above cursor
          pi
      %^  prod:cy  cur.pic  des.pic
      |=  =node
      ^+  node
      ?.  ?=(%tree -.node)  node
      =.  vew.node  vew
      ?.  vew  node
      =-  ~&  [%new-dir pax.node -]
          node(dir -)
      =/  full=path  pax.node
      =/  targ=path
        ?>  ?=(^ full)
        [(scot %p our.bowl) i.full (scot %da now.bowl) t.full]
      =/  =arch
        .^(arch %cy targ)
      ?:  !=(~ fil.arch)
        ~|  %shouldve-caught-earlier
        !!
      %-  zing
      %+  turn  (sort ~(tap by dir.arch) aor)
      ::  we have previously established this dir contains multiple things. but:
      ::  for everything _under_ this dir, if it contains:
      ::  - just a file: flatten.
      ::  - just one dir: flatten.
      ::  - multiple dirs and/or file: end flatness.
      ::
      |=  [dir=@ta ~]
      ^-  (list ^node)
      =.  full  (snoc full dir)
      =.  targ  (snoc targ dir)
      =/  arch  .^(^arch %cy targ)
      ?+  arch  ~|(%ungo-bungo !!)
        [~ ~]        ~&([%huh-empty-dir full] ~)
        [^ ~]        [%file full]~
        [^ ^]        ~[[%file full] [%tree full | ~]]
        [~ [^ ~ ~]]  $(dir p.n.dir.arch)
        [~ [^ * *]]  [%tree full | ~]~
      ==
    ::
    ++  pi-draw
      ~>  %bout.[0 'uno-pi-draw']
      =-  pi(biz [[%mor [%clr ~] (snoc - [%hop w.size.ses 0])] biz])
      ::TODO  want to:
      ::  1) skip path nodes out of view
      ::  2) while nodes are in view:
      ::    3) if it's a file, draw the node
      ::    4) if it's a directory, draw the node
      ::    5) if the directory is open, go to 2 with new base
      ::    6) continue
      ::
      =-  ~&  xx+(lent -)  -
      %-  tail
      %+  roll  des.pic
      |=  [nod=node [[mis=_off.pic dep=@ud top=@ud yyy=@ud] out=(list blit)]]
      ^+  +<+
      ?:  (gte yyy h.size.ses)  +<+
      =*  klr=stye  [~ ?:(=(yyy (sub cur.pic off.pic)) [%k %w] [~ ~])]
      ::TODO  if =(0 dep) draw as desk instead
      ?-  -.nod
          %file
        ?.  =(0 mis)
          [[(dec mis) dep top yyy] out]
        ?:  (gte yyy h.size.ses)
          [[mis dep top yyy] out]
        :-  [0 dep top +(yyy)]
        ~&  [%render-f for=pax.nod]
        =-  [[%mor [%hop 0 yyy] [%klr [-]~] ~] out]
        :-  klr
        (tuba (weld (reap (mul top 2) ' ') '⋅' ' ' (spud (slag top pax.nod))))
      ::
          %tree
        ?.  vew.nod
          ?.  =(0 mis)
            [[(dec mis) dep top yyy] out]
          ?:  (gte yyy h.size.ses)
            [[mis dep top yyy] out]
          :-  [0 dep top +(yyy)]
          ~&  [%render-t for=pax.nod]
          =-  [[%mor [%hop 0 yyy] [%klr [-]~] ~] out]
          :-  klr
          (tuba (weld (reap (mul top 2) ' ') '⮸' ' ' (spud (slag top pax.nod))))
        ?>  ?=(^ dir.nod)
        =/  len  (lent pax.nod)
        |-
        ?.  =(0 mis)
          =/  new
            %_  ^$
              mis  (dec mis)
              nod  i.dir.nod
              top  len
              dep  +(dep)
              mis  (dec mis)
            ==
          =.  mis  mis.new
          =.  yyy  yyy.new
          =.  out  out.new
          ?:  (gte yyy h.size.ses)
            [[mis dep top yyy] out]
          ?~  t.dir.nod
            [[mis dep top yyy] out]
          $(dir.nod t.dir.nod)
        =.  out
          ~&  [%render-d for=pax.nod]
          =-  [[%mor [%hop 0 yyy] [%klr [-]~] ~] out]
          :-  klr
          (tuba (weld (reap (mul top 2) ' ') '⮶' ' ' (spud (slag top pax.nod))))
        =.  yyy  +(yyy)
        |-
        =/  new  ^^$(nod i.dir.nod, dep +(dep), top len)
        =.  mis  mis.new
        =.  yyy  yyy.new
        =.  out  out.new
        ?:  (gte yyy h.size.ses)
          [[mis dep top yyy] out]
        ?~  t.dir.nod
          [[mis dep top yyy] out]
        $(dir.nod t.dir.nod)
      ==
    ::
    ++  pi-path  ::  selected filepath (with leading desk) if any
      ^-  [fil=? pax=path]
      =/  =node  (snag:cy cur.pic des.pic)
      ?-  -.node
        %file  &^pax.node
        %tree  |^pax.node
      ==
    --
  ::  +bu: filebuffer engine
  ::
  ++  bu
    |_  [buf=buffer qit=_|]
    +*  bu  .
    ::
    ++  bu-apex
      |=  b=buffer
      bu(buf b)
    ::
    ++  bu-abet
      ^+  se
      ?:  qit  se-draw:se-pick
      se(what.ses [%buffer `buffer`buf])
    ::
    ++  bu-bell  bu(biz [[%bel ~] biz])
    ++  bu-show  |=(=@t bu(info.ses `t))
    ++  bu-moan  bu-show:bu-bell
    ::
    ++  bu-load
      =<  bu-draw:bu-melt
      =/  bas  /(scot %p our.bowl)/[desk.bat.buf]/(scot %da now.bowl)
      =/  pas  (weld bas spur.bat.buf)
      =/  mas  (rear spur.bat.buf)
      =/  cas  .^(cass:clay %cw bas)
      =/  has  .^(? %cu pas)
      =;  =wain
        %_  bu
          fro.buf  ?:(has `[ud.cas (mug wain)] ~)
          buf.buf  wain
        ::
            pos.buf
          =.  l.pos.buf  (min l.pos.buf (dec (max 1 (lent wain))))
          =.  c.pos.buf  (min c.pos.buf (met 3 (snag l.pos.buf wain)))
          pos.buf  ::TODO  readjust vew.buf also?
        ::
            info.ses
          =;  pad=@t  `(cat 3 'loaded ' pad)
          (spat (weld bas(+14 (scot %ud ud.cas)) spur.bat.buf))
        ==
        :: `(list [wid=@ud lin=(list @t)])`(turn wain |=(l=@t [0 l ~]))
      ?.  has
        ::  file does not exist yet, empty buffer
        ::
        ['']~
      ?:  =(%txt mas)   .^(wain %cx pas)
      ?:  =(%hoon mas)  (snoc (to-wain:format .^(@t %cx pas)) '')
      ::TODO  want to detect tube scry failures ahead of time somehow?
      !<  wain
      %.  .^(vase %cr pas)
      .^(tube:clay %cc (weld bas /[mas]/txt))
    ::
    ++  bu-size
      bu-draw:bu-melt
    ::
    ++  bu-belt
      |=  =belt:dill
      ^+  bu
      =.  info.ses  ~
      ?^  act.buf
        ?+  belt
            =;  [new=@t *]  bu-cure:bu-mess(t.act.buf new)
            (generic-input:etui [t.act.buf (met 3 t.act.buf)] belt)
        ::
            [%ret ~]
          ?-  -.act.buf
            %find  (bu-find(fin.buf t.act.buf, act.buf ~) t.act.buf)
          ::
              %line
            ?~  lin=(rush t.act.buf ;~(pose dum:ag dem:ag))
              bu-bell
            (bu-goto(act.buf ~) u.lin)
          ==
        ==
      ::
      =<  bu-cure:bu-mess  ::TODO  only if needed?
      =*  wid=@ud  (met 3 (snag l.pos.buf buf.buf))
        ::TODO  do properly, via snag:wrap:etui or w/e
        :: =/  lin=(list @t)  lin:(snag l.pos.buf buf.buf)
        :: ?~(lin '' i.lin)
      =*  hyt=@ud  (lent buf.buf)
        :: (lent:wrap:etui buf.buf)
      ::
      ?:  ?=([%mod %ctl ?(%a %e)] belt)
        =/  n=@ud  ?-(key.belt %a 0, %e wid)
        bu-look(pos.buf [[n l.pos.buf] n])
      ::
      ?:  ?=([%mod %ctl *] belt)
        =*  cas  ~+
          =/  bas  /(scot %p our.bowl)/[desk.bat.buf]/(scot %da now.bowl)
          .^(cass:clay %cw bas)
        =*  saf=?
          ?~  fro.buf  &
          =(cas.u.fro.buf ud.cas)
        ?+  key.belt  bu-bell
          %p  bu(biz [[%sav bat.buf bu-fyl] biz])
          %q  bu(qit &)  ::TODO  safeguard if changes
          %r  bu-load  ::TODO  safeguard if changes
          %f  bu(act.buf [%find fin.buf])
          %g  bu(act.buf [%line ''])
        ::
            ?(%s %'S')
          =/  saf=?  |(=('S' key.belt) saf)
          ?.  saf
            (bu-moan 'file changed since load. ctl+S to overwrite anyway.')
          %.  'saved!'
          %_  bu-show
            fro.buf  `[+(ud.cas) (mug buf.buf)]
          ::
              caz
            :_  caz
            =/  =mark
              =+  m=(rear spur.bat.buf)
              ?:(?=(?(%txt %hoon) m) m %txt)
            =/  =cage
              :-  mark
              ?+  mark  !!
                %txt   !>(buf.buf)
                %hoon  !>(bu-fyl)
              ==
            =/  =miso:clay
              ?~(fro.buf [%ins cage] [%mut cage])
            =+  [%info desk.bat.buf %& [spur.bat.buf miso]~]
            [%pass [%save sid [desk spur]:bat.buf] %arvo %c -]
          ==
        ==
      ::
      ?:  ?=([%aro *] belt)
        =;  new=[c=@ud l=@ud]
          =/  ned=@ud
            ?:(?=(?(%l %r) p.belt) c.new d.pos.buf)
          bu-look(pos.buf [new ned])
        ?-  p.belt
          %u  ?:  =(0 l.pos.buf)  [0 0]
              =.  l.pos.buf  (dec l.pos.buf)
              [(min d.pos.buf wid) l.pos.buf]
          %d  =/  end=?  =(hyt +(l.pos.buf))
              ?:  end  [wid l.pos.buf]
              =.  l.pos.buf  +(l.pos.buf)
              [(min d.pos.buf wid) l.pos.buf]
          %l  ?.  =(0 c.pos.buf)  [(dec c.pos.buf) l.pos.buf]
              ?:  =(0 l.pos.buf)  [0 0]
              =.  l.pos.buf  (dec l.pos.buf)
              [wid l.pos.buf]
          %r  ?.  =(wid c.pos.buf)  [+(c) l]:pos.buf
              =/  end=?  =(hyt +(l.pos.buf))
              ?:(end -.pos.buf [0 +(l.pos.buf)])
        ==
      ::
      =;  [[new=[? @ud] pus=_-.pos.buf] nuf=_buf.buf]
        =.  pos.buf  [pus c.pus]
        =.  buf.buf  nuf
        ::TODO  partial redraw: %& just the line, %| also downward
        ::TODO  bu-look might also redraw though...
        bu-buff:bu-look
      :-  ::  move cursor based on edit
          ::
          ?+  belt  [|+0 -.pos.buf]  ::TODO
            @         [&+l +(c) l]:pos.buf
            [%ret ~]  [|+l 0 +(l)]:pos.buf
            [%bac ~]  =,  pos.buf
                      ?.  =(0 c.pos.buf)  [&+l (dec c) l]
                      =.  l.pos.buf  (dec (max 1 l))
                      [|+l wid l]
            [%del ~]  :_  -.pos.buf
                      [=((met 3 (snag l.pos.buf buf.buf)) c.pos.buf) l.pos.buf]
            [%hit *]  :-  &+0
                      ?:  (gth y.belt (sub h.size.ses 4))  -.pos.buf
                      =/  new  (add:co:etui vew.buf +.belt)
                      =.  x.new  (min x.new (met 3 (snag y.new buf.buf)))
                      new
          ==
      ::  apply edit to buffer
      ::
      ?:  ?=([%hit *] belt)  buf.buf
      =/  y=@ud  l.pos.buf
      |-  ^+  buf.buf
      ?~  buf.buf  ~&([%cursor-beyond-buf l.pos.buf] !!)
      ?.  =(0 y)
        ?.  ?&(=(1 y) =(0 c.pos.buf) ?=([%bac ~] belt))
          [i.buf.buf $(y (dec y), buf.buf t.buf.buf)]
        ?>  ?=(^ t.buf.buf)
        [(cat 3 [i i.t]:buf.buf) t.t.buf.buf]
      ?+  belt  ~&([%wasted-effort belt] buf.buf)
        @         :_  t.buf.buf
                  %+  rap  3
                  :~  (end 3^c.pos.buf i.buf.buf)
                      belt
                      (rsh 3^c.pos.buf i.buf.buf)
                  ==
        [%txt *]  :_  t.buf.buf
                  %+  rap  3
                  :~  (end 3^c.pos.buf i.buf.buf)
                      (crip (turn p.belt tuft))
                      (rsh 3^c.pos.buf i.buf.buf)
                  ==
        [%ret ~]  :+  (end 3^c.pos.buf i.buf.buf)
                    (rsh 3^c.pos.buf i.buf.buf)
                  t.buf.buf
        [%bac ~]  ?:  =(0 c.pos.buf)  buf.buf
                  :_  t.buf.buf
                  %^  cat  3
                    (end 3^(dec c.pos.buf) i.buf.buf)
                  (rsh 3^c.pos.buf i.buf.buf)
        [%del ~]  ?:  =((met 3 i.buf.buf) c.pos.buf)
                    ?~  t.buf.buf  buf.buf
                    [(cat 3 [i i.t]:buf.buf) t.t.buf.buf]
                  :_  t.buf.buf
                  %^  cat  3
                    (end 3^c.pos.buf i.buf.buf)
                  (rsh 3^+(c.pos.buf) i.buf.buf)
      ==
      ::
      ::NOTE  soft-wrapped buffer logic below
      :: ?^  belt  bu-draw
      :: ::
      :: ::TODO  if text input, also update c & d .pos
      :: =;  [new=(each @ud @ud) nuf=_buf.buf]
      ::   =.  buf.buf  nuf
      ::   =.  pos.buf
      ::     ::TODO  account for more complex cases
      ::     [[+(c) l] +(c)]:pos.buf
      ::   bu-draw  ::TODO  redraw according to :new, %& one line, or %| downward
      :: ::NOTE  partially copied from book's +di-blit %wyp logic...
      :: ::  walk down the buffer until we reach the line to edit,
      :: ::  then modify it, pushing content onto the next visual line
      :: ::  if necessary.
      :: ::
      :: :-  |+0  ::TODO
      :: =/  y=@ud  l.pos.buf
      :: |-  ^+  buf.buf
      :: ?~  buf.buf  ~&(%xx !!)
      :: ?:  =(~ lin.i.buf.buf)
      ::   ?:  =(0 y)  [[wid.i.buf.buf [`@`belt]~] t.buf.buf]
      ::   [i.buf.buf $(buf.buf t.buf.buf, y (dec y))]
      :: =|  but=(list @t)
      :: |-
      :: ::TODO  excessive flopping. maybe we want to (lent) instead?
      :: ?~  lin.i.buf.buf
      ::   ::  maintain the lines we just passed
      ::   ::
      ::   [[wid.i.buf.buf (flop but)] ^$(buf.buf t.buf.buf)]
      :: ?.  =(0 y)
      ::   ::  store a visual line
      ::   ::
      ::   $(but [i.lin.i.buf.buf but], lin.i.buf.buf t.lin.i.buf.buf, y (dec y))
      :: =/  wid=@ud  (met 3 i.lin.i.buf.buf)
      :: ?:  (lth wid wid.i.buf.buf)
      ::   =;  new=@t
      ::     [[wid.i.buf.buf (weld (flop but) [new t.lin.i.buf.buf])] t.buf.buf]
      ::   %+  rap  3
      ::   :~  (end 3^c.pos.buf i.lin.i.buf.buf)
      ::       belt
      ::       (rsh 3^c.pos.buf i.lin.i.buf.buf)
      ::   ==
      :: ~&  %too-big-lol  ::TODO
      :: [[wid.i.buf.buf (weld (flop but) lin.i.buf.buf)] t.buf.buf]
    ::
    ++  bu-find
      |=  t=@t
      ^+  bu
      ~>  %bout.[0 'bu-find']
      =/  find  (cury find (trip t))
      |^  =/  res=(unit [c=@ud l=@ud])
            =/  r  (seek l.pos.buf (slag l.pos.buf buf.buf))
            ?~  r  (seek 0 (scag l.pos.buf buf.buf))
            r
          ?~  res
            bu-mess:bu-bell(info.ses `'not found')
          bu-draw:bu-look(pos.buf [[c l] c]:u.res)
      ::
      ++  seek
        |=  [num=@ud buf=wain]
        ^-  (unit [c=@ud l=@ud])
        ?~  buf  ~
        ?~  res=(find (trip i.buf))
          $(num +(num), buf t.buf)
        ?:  =([u.res num] -.pos.^buf)
          ::TODO  feels like a weird hack. why can't you just be normal?
          $(i.buf (can 3 +(u.res)^~ u.res^(rsh 3^+(u.res) i.buf) ~))
        `[u.res num]
      --
    ::
    ++  bu-goto
      |=  l=@ud
      ^+  bu
      =.  l  (min (dec (max 1 l)) (lent buf.buf))
      =.  c.pos.buf  (min c.pos.buf (met 3 (snag l buf.buf)))
      =.  l.pos.buf  l
      bu-draw:bu-look
    ::
    ++  bu-draw
      bu-cure:bu-mess:bu-foot:bu-buff
    ::
    ++  bu-buff  ::  draw buffer
      =;  bis=(list blit)  bu(biz [[%mor bis] biz])
      =/  h=@ud    (sub h.size.ses 3)
      =.  buf.buf  (slag y.vew.buf buf.buf) ::(generic-scag:etui vew.buf buf.buf)
      =/  y=@ud    0
      |-
      ?:  =(y h)   ~
      ?~  buf.buf
        [[%hop 0 y] [%wyp ~] $(y +(y))]
      :+  [%hop 0 y]
        :-  %put
        %+  rip  3
        =/  lin=@t   (rsh 3^x.vew.buf i.buf.buf)
        =/  wid=@ud  (met 3 lin)
        ?:  (gth wid w.size.ses)  (end 3^w.size.ses lin)
        (cat 3 lin (fil 3 (sub w.size.ses (met 3 lin)) ' '))
      $(y +(y), buf.buf t.buf.buf)
      ::
      ::NOTE  soft-wrapped buffer logic below
      :: ?.  =(wid.i.buf.buf (dec w.size.ses))
      ::   ~&  [dap.bowl %unclean-buffer have=wid.i.buf.buf need=(dec w.size.ses)]
      ::   !!
      :: ?~  lin.i.buf.buf
      ::   :+  [%hop 0 y]
      ::     [%put (reap w.size.ses ~-.)]
      ::   $(y +(y), buf.buf t.buf.buf)
      :: |-
      :: ?:  =(y h)  ~
      :: :+  [%hop 0 y]
      ::   :-  %put
      ::   ::  a line follows, so we know this one is "full"
      ::   ::
      ::   ?^  t.lin.i.buf.buf
      ::     (rip 3 i.lin.i.buf.buf)
      ::   ::  no line follows, make sure we right-pad with spaces
      ::   ::
      ::   %+  rip  3
      ::   %^  cat  3
      ::     i.lin.i.buf.buf
      ::   (fil 3 (sub w.size.ses (met 3 i.lin.i.buf.buf)) ' ')
      :: ::
      :: ?~  t.lin.i.buf.buf
      ::   ^$(y +(y), buf.buf t.buf.buf)
      :: $(y +(y), lin.i.buf.buf t.lin.i.buf.buf)
    ::
    ++  bu-mess
      =;  bis=(list blit)  bu(biz [[%mor bis] biz])
      =/  info=(list @)
        ?~  act.buf
          (trip (end 3^w.size.ses (fall info.ses ' - - - ')))
        =-  (trip (cat 3 - t.act.buf))
        ?-  -.act.buf
          %find  'find: '
          %line  'goto: '
        ==
      =.  info
        =+  wid=(lent info)
        ?:  =(wid w.size.ses)  info
        ?:  (gth wid w.size.ses)
          (scag w.size.ses info)
        (weld info (reap (sub w.size.ses wid) ' '))
      :~  [%hop 0 (sub h.size.ses 3)]
          [%klr [[~ %k %w] info] ~]
      ==
    ::
    ++  bu-foot
      =;  bis=(list blit)  bu(biz [[%mor bis] biz])
      :~  [%hop 0 (sub h.size.ses 2)]
          :-  %klr
          %+  scag:klr:format  w.size.ses
          :~  [[~ %k %w] `(list @)`" ctl+s "]
              [[~ ~ ~] `(list @)`" save     "]
              [[~ %k %w] `(list @)`" ctl+p "]
              [[~ ~ ~] `(list @)`" download "]
              [[~ %k %w] `(list @)`" ctl+f "]
              [[~ ~ ~] `(list @)`" search   "]
              [[~ ~ ~] (reap w.size.ses ~-.)]
          ==
        ::
          [%hop 0 (sub h.size.ses 1)]
          :-  %klr
          %+  scag:klr:format  w.size.ses
          :~  [[~ %k %w] `(list @)`" ctl+r "]
              [[~ ~ ~] `(list @)`" reload   "]
              [[~ %k %w] `(list @)`" ctl+q "]
              [[~ ~ ~] `(list @)`" close    "]
              [[~ %k %w] `(list @)`" ctl+g "]
              [[~ ~ ~] `(list @)`" to line  "]
              [[~ ~ ~] (reap w.size.ses ~-.)]
          ==
      ==
    ::
    ++  bu-cure  ::  position cursor
      =;  =spot:etui
        bu(biz [[%hop spot] biz])
      ?^  act.buf
        [(add 6 (met 3 t.act.buf)) (sub h.size.ses 3)]
      ~|  [pos=-.pos.buf vew=vew.buf]
      (sub:co:etui -.pos.buf vew.buf)
    ::
    ++  bu-look  ::  adjust viewport to keep cursor in view
      =+  ole=vew.buf
      =.  y.vew.buf
        =*  l  l.pos.buf
        ?:  (lth l y.vew.buf)
          l
        =.  h.size.ses  (sub h.size.ses 3)  ::  status bar
        ?:  (gth l (dec (add y.vew.buf h.size.ses)))
          (sub l (dec h.size.ses))
        y.vew.buf
      =.  x.vew.buf
        =*  c  c.pos.buf
        ?:  (lth c x.vew.buf)
          c
        ?:  (gth c (dec (add x.vew.buf w.size.ses)))
          (sub c (dec w.size.ses))
        x.vew.buf
      ?:  =(ole vew.buf)  bu
      bu-buff
    ::
    ++  bu-melt
      ^+  bu
      bu
      :: ~>  %bout.[0 ' uno: bu-melt']
      :: =-  bu(buf.buf -)
      :: %:  generic-melt:etui
      ::   (dec w.size.ses)
      ::   ::NOTE  full melt makes things easier. but how slow for big buff?
      ::   [0 ~]  :: [vew.buf `h.size.ses]
      ::   [(cury rap 3) |=([x=@ud l=@t] [(end 3^x l) (rsh 3^x l)]) same]
      ::   buf.buf
      :: ==
    ::
    ++  bu-fyl
      ^-  @t
      ::TODO  ensure trailing newline
      (of-wain:format buf.buf)
    --
  --
:: ++  flatten-nodes
::   ^-  (list bath)
::   %+  turn  fis
::   |=  =node
::   :-  nom
::   ?.  ?=([%& %& *] low.node)  [nom /]
::   (turn dir.p.low.node |=)
:: ::
:: ++  selected-bath
::   ^-  bath
::   =;  nos=(list @ta)
::     [- +]:(flop nos)
::     :: =.  nos  (flop nos)
::     :: ?~  nos  !!
::     :: [i.nos t.nos]
::   |-
::   =/  nod=node  (snag i.pos fis)
::   :-  nom.nod
::   ?~  t.pos  ~
::   =.  pos  t.pos
::   $(pos t.pos, fis dir.fis)
::
:: ++  selected-path
::   |=  [fis=(list node:x) pos=@ud]
::   =|  nos=(list @ta)
::   |-  ^+  [nos pos]
::   ?~  fis  [~ pos]
::   ?:  =(0 pos)
::     [[nom.i.fis nos] pos]
::   ::  step to the next visible node
::   ::
::   =.  pos  (dec pos)
::   ?@  low.i.fis
::     $(fis t.fis)
::   ?.  vew.low.i.fis
::     $(fis t.fis)
::   =^  res  pos  $(fis dir.low.i.fis, nos [nom.i.fis nos])
::   ?~(res $(fis t.fis) [res pos])
--
