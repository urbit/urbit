::  demo-slides
::
/+  dbug, verb, default-agent
::
|%
+$  state-0
  $:  %0
      ses=@ta
      slide=@ud
      point=(map @udy @udx)
  ==
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
    [~ this]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?.  ?=(%dill-poke mark)  (on-poke:def mark vase)
    =+  !<([ses=@ta belt=dill-belt:dill] vase)
    =?  slide  ?=(?(%' ' [%aro *]) belt)
      ?+  belt  slide
        [%aro %l]          (dec (max 1 slide))
        ?(%' ' [%aro %r])  (min +(slide) (dec (lent slides)))
      ==
    =?  point  ?=([%aro *] belt)
      ~
    =?  point  ?=([%hit *] belt)
      (~(put by *(map @udy @udx)) [y x]:belt)
    =?  point  ?=([%mod %met %hit *] belt)
      (~(put by point) [y x]:key.belt)
    [[render]~ this]
  ::
  ++  on-watch
    |=  =path
    ?.  ?=([%dill @ ~] path)  (on-watch:def path)
    =.  ses  i.t.path
    [[render]~ this]
  ::
  ++  on-agent  on-agent:def
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|%
++  render
  ^-  card
  =;  =blit
    [%give %fact [/dill/[ses]]~ %dill-blit !>(blit)]
  :-  %mor
  :*  [%clr ~]  ::NOTE  causes flickers in bare sessions
      [%hop 0 0]
      [%put `(list @)`"{(a-co:co +(slide))}/{(a-co:co (lent slides))}                                                                                                                       "]
    ::
      =-  (flop out)
      %+  roll
        =+  lis=(snag slide slides)
        =+  len=(lent lis)
        ?.  (lth len 28)  lis
        %+  weld  lis
        `(list stub)`(reap (sub 28 len) `stub`[*stye (reap 128 ~-.)]~)
      |=  [lin=stub row=_1 out=(list blit)]
      :-  +(row)
      =?  lin  (~(has by point) row)
        =/  x=@ud  (~(got by point) row)
        =/  [h=stub t=stub]  (trim:klr (~(got by point) row) lin)
        %+  weld  h
        ?~  t  t
        ^-  stub
        [[[[%br ~ ~] [0xbb 0xff 0xbb] ~] q.i.t] t.t]
      [[%klr lin] [%hop 0 row] out]
  ==
::
++  make
  |=  c=$@(@t (list $@(@t [~ c=@t])))
  ^-  stub
  ?@  c  [*stye (tuba (trip c))]~
  %+  turn  c
  |=  c=$@(@t [~ c=@t])
  ?@  c  [*stye (tuba (trip c))]
  :-  [[%br ~ ~] ~ [0x0 0x0 0xff]]
  (tuba (trip c.c))
::
++  slides  ^~
  ::TODO  make sure width and height is always filled with spaces, for redraws
  ^-  (list (list stub))
  :~
::
=;  l=(list $@(@t (list $@(@t [~ @t]))))  (turn l make)
:~
'                                                                                                                                '
'                                                                                                                                '
~[`'                                                     terminal stack architecture                                                ']
'                                                                                                                                '
'                                                         with ~palfun-foslup                                                    '
'                                                                                                                                '
'                                                                                                                                '
'                                                                                                                                '
==
=;  l=(list $@(@t (list $@(@t [~ @t]))))  (turn l make)
:~
'                                                                                                                                '
'                ┌────────────────────┐                                                                                          '
'                │                    │                                                                                          '
'                │      runtime       │                                                                                          '
'                │                    │                                                                                          '
'                └────────────────────┘                                                                                          '
'                                                                                                                                '
'                                                                                                                                '
'                ┌────────────────────┐                                                                                          '
'                │                    │                                                                                          '
'                │      kernel        │                                                                                          '
'                │                    │                                                                                          '
'                └────────────────────┘                                                                                          '
'                                                                                                                                '
'                                                                                                                                '
'                ┌────────────────────┐                                                                                          '
'                │                    │                                                                                          '
'                │     userspace      │                                                                                          '
'                │                    │                                                                                          '
'                └────────────────────┘                                                                                          '
'                                                                                                                                '
'                                                                                                                                '
==
=;  l=(list $@(@t (list $@(@t [~ @t]))))  (turn l make)
:~
'                                                                                                                                '
'                ┌────────────────────┐                                                                                          '
'                │                    │                                                                                          '
~['                │  ' `'runtime (term.c)' '  │                                                                                          ']
'                │                    │                                                                                          '
~['                └───┬────────────' `'▲' '───┘                                                                                          ']
~[`'       input events │            │ output effects                                                                               ']
~[`'                    │            │                                                                                              ']
~['                ┌───' `'▼' '────────────┴───┐                                                                                          ']
'                │                    │                                                                                          '
'                │      kernel        │                                                                                          '
'                │                    │                                                                                          '
'                └────────────────────┘                                                                                          '
'                                                                                                                                '
'                                                                                                                                '
'                ┌────────────────────┐                                                                                          '
'                │                    │                                                                                          '
'                │     userspace      │                                                                                          '
'                │                    │                                                                                          '
'                └────────────────────┘                                                                                          '
'                                                                                                                                '
==
=;  l=(list $@(@t (list $@(@t [~ @t]))))  (turn l make)
:~
'                                                                                                                                '
'                ┌────────────────────┐                                                                                          '
'                │                    │                                                                                          '
'                │  runtime (term.c)  │                                                                                          '
'                │                    │                                                                                          '
'                └───┬────────────▲───┘                                                                                          '
'       input events │            │ output effects                                                                               '
'                    │            │                                                                                              '
'                ┌───▼────────────┴───┐                                                                                          '
'                │                    │                                                                                          '
~['                │   ' `'kernel (dill)' '    │                                                                                          ']
'                │                    │                                                                                          '
'                └────────────────────┘                                                                                          '
'                                                                                                                                '
'                                                                                                                                '
'                ┌────────────────────┐                                                                                          '
'                │                    │                                                                                          '
'                │     userspace      │                                                                                          '
'                │                    │                                                                                          '
'                └────────────────────┘                                                                                          '
'                                                                                                                                '
==
=;  l=(list $@(@t (list $@(@t [~ @t]))))  (turn l make)
:~
'                                                                                                                                '
'                ┌────────────────────┐                                                                                          '
'                │                    │                                                                                          '
'                │  runtime (term.c)  │                                                                                          '
'                │                    │                                                                                          '
'                └───┬────────────▲───┘                                                                                          '
'       input events │            │ output effects                                                                               '
'                    │            │                                                                                              '
'                ┌───▼────────────┴───┐                                                                                          '
'                │                    │                                                                                          '
'                │   kernel (dill)    │                                                                                          '
'                │                    │                                                                                          '
~['                └───┬────────────' `'▲' '───┘                                                                                          ']
~[`'        input pokes │            │ output facts                                                                                 ']
~[`'                    │            │                                                                                              ']
~['                ┌───' `'▼' '────────────┴───┐                                                                                          ']
'                │                    │                                                                                          '
~['                │  ' `'userspace agent' '   │                                                                                          ']
'                │                    │                                                                                          '
'                └────────────────────┘                                                                                          '
'                                                                                                                                '
==
=;  l=(list $@(@t (list $@(@t [~ @t]))))  (turn l make)
:~
'                                                                                                                                '
~['                ┌────────────────────┐' `'              ┌────────────────────┐                                                      ']
~['                │                    │' `'              │                    │                                                      ']
~['                │  runtime (term.c)  │' `'              │    web terminal    │                                                      ']
~['                │                    │' `'              │                    │                                                      ']
~['                └───┬────────────▲───┘' `'              └───┬────────────▲───┘                                                      ']
~['       input events │            │ output effects' `'       │input pokes │output facts                                              ']
~['                    │            │    ' `'                  │            │                                                          ']
~['                ┌───▼────────────┴───┐' `'              ┌───▼────────────┴───┐                                                      ']
~['                │                    ├' `'──────────────►                    │                                                      ']
~['                │   kernel (dill)    │' `'input & output│ proxy agent (herm) │                                                      ']
~['                │                    ' `'◄──────────────┤                    │                                                      ']
~['                └───┬────────────▲───┘' `'              └────────────────────┘                                                      ']
'        input pokes │            │ output facts                                                                                 '
'                    │            │                                                                                              '
'                ┌───▼────────────┴───┐                                                                                          '
'                │                    │                                                                                          '
'                │  userspace agent   │                                                                                          '
'                │                    │                                                                                          '
'                └────────────────────┘                                                                                          '
'                                                                                                                                '
==
=;  l=(list $@(@t (list $@(@t [~ @t]))))  (turn l make)
:~
'                                                                                                                                '
'                ┌────────────────────┐              ┌────────────────────┐                                                      '
'                │                    │              │                    │                                                      '
'                │  runtime (term.c)  │              │    web terminal    │                                                      '
'                │                    │              │                    │                                                      '
'                └───┬────────────▲───┘              └───┬────────────▲───┘                                                      '
'       input events │            │ output effects       │input pokes │output facts                                              '
'                    │            │                      │            │                                                          '
'                ┌───▼────────────┴───┐              ┌───▼────────────┴───┐                                                      '
'                │                    ├──────────────►                    │                                                      '
'                │   kernel (dill)    │input & output│ proxy agent (herm) │                                                      '
'                │                    ◄──────────────┤                    │                                                      '
'                └───┬────────────▲───┘              └────────────────────┘                                                      '
'        input pokes │            │ output facts                                                                                 '
'                    │            │                                                                                              '
'                ┌───▼────────────┴───┐                                                                                          '
'                │                    │                                                                                          '
~['                │  ' `'userspace (drum)' '  │                                                                                          ']
'                │                    │                                                                                          '
~['                └───┬────────────' `'▲' '───┘                                                                                          ']
~[`'        input pokes │            │ output facts                                                                                 ']
~[`'                    │            │                                                                                              ']
~[`'                ┌───▼────────────┴───┐                                                                                          ']
~[`'                │                    │                                                                                          ']
~[`'                │    other agents    │                                                                                          ']
~[`'                │                    │                                                                                          ']
~[`'                └────────────────────┘                                                                                          ']
'                                                                                                                                '
==
=;  l=(list $@(@t (list $@(@t [~ @t]))))  (turn l make)
:~
'                                                                                                                                '
'                ┌────────────────────┐                 +$  bolt                                     ::  simple input            '
'                │                    │                   $@  @c                                     ::  simple keystroke        '
'                │  runtime (term.c)  │                   $%  [%aro p=?(%d %l %r %u)]                ::  arrow key               '
'                │                    │                       [%bac ~]                               ::  true backspace          '
'                └───┬────────────▲───┘                       [%del ~]                               ::  true delete             '
~[`'       input events' ' │            │ output effects            [%hit x=@ud y=@ud]                     ::  mouse click             ']
~[`'              %belt' ' │            │                           [%ret ~]                               ::  return                  ']
'                ┌───▼────────────┴───┐                   ==                                         ::                          '
'                │                    │                 +$  belt                                     ::  client input            '
'                │   kernel (dill)    │                   $?  bolt                                   ::  simple input            '
'                │                    │                       [%mod mod=?(%ctl %met %hyp) key=bolt]  ::  w/ modifier             '
'                └───┬────────────▲───┘                       [%txt p=(list @c)]                     ::  utf32 text              '
~[`'        input pokes' ' │            │ output facts          ==                                         ::                          ']
~[`'         %dill-belt' ' │            │                     +$  dill-belt                                ::  arvo input              ']
'                ┌───▼────────────┴───┐                   $%  belt                                   ::  client input            '
'                │                    │                       [%hey ~]                               ::  refresh                 '
'                │  userspace (drum)  │                       [%rez p=@ud q=@ud]                     ::  resize, cols, rows      '
'                │                    │                       [%yow p=gill:gall]                     ::  connect to app          '
'                └───┬────────────▲───┘                   ==                                         ::                          '
'        input pokes │            │ output facts                                                                                 '
'                    │            │                                                                                              '
'                ┌───▼────────────┴───┐                                                                                          '
'                │                    │                                                                                          '
'                │    other agents    │                                                                                          '
'                │                    │                                                                                          '
'                └────────────────────┘                                                                                          '
'                                                                                                                                '
==
=;  l=(list $@(@t (list $@(@t [~ @t]))))  (turn l make)
:~
'                                                                                                                                '
'                ┌────────────────────┐                 %watch /dill/[session-id]                                                '
'                │                    │                                                                                          '
'                │  runtime (term.c)  │                 +$  blit                                     ::  client output           '
'                │                    │                   $%  [%bel ~]                               ::  make a noise            '
'                └───┬────────────▲───┘                       [%clr ~]                               ::  clear the screen        '
~['       input events │            │ ' `'output effects' '            [%hop p=$@(@ud [x=@ud y=@ud])]         ::  set cursor col/pos      ']
~['              %belt │            │ ' `'%blit         ' '            [%klr p=stub]                          ::  put styled              ']
'                ┌───▼────────────┴───┐                       [%mor p=(list blit)]                   ::  multiple blits          '
'                │                    │                       [%nel ~]                               ::  newline                 '
'                │   kernel (dill)    │                       [%put p=(list @c)]                     ::  put text at cursor      '
'                │                    │                       [%sag p=path q=*]                      ::  save to jamfile         '
'                └───┬────────────▲───┘                       [%sav p=path q=@]                      ::  save to file            '
~['        input pokes │            │ ' `'output facts  ' '            [%url p=@t]                            ::  activate url            ']
~['         %dill-belt │            │ ' `'%dill-blit    ' '            [%wyp ~]                               ::  wipe cursor line        ']
'                ┌───▼────────────┴───┐                   ==                                         ::                          '
'                │                    │                 +$  dill-blit                                ::  arvo output             '
'                │  userspace (drum)  │                   $%  blit                                   ::  client output           '
'                │                    │                       [%qit ~]                               ::  close console           '
'                └───┬────────────▲───┘                   ==                                         ::                          '
'        input pokes │            │ output facts                                                                                 '
'                    │            │                                                                                              '
'                ┌───▼────────────┴───┐                                                                                          '
'                │                    │                                                                                          '
'                │    other agents    │                                                                                          '
'                │                    │                                                                                          '
'                └────────────────────┘                                                                                          '
'                                                                                                                                '
==
=;  l=(list $@(@t (list $@(@t [~ @t]))))  (turn l make)
:~
'                                                                                                                                '
'                ┌────────────────────┐                 +$  sole-id  [who=@p ses=@ta]                ::  session id              '
'                │                    │                 +$  sole-action                              ::  sole to app             '
'                │  runtime (term.c)  │                   $:  id=sole-id                             ::  session id              '
'                │                    │                     $=  dat                                  ::  action:                 '
'                └───┬────────────▲───┘                     $%  [%det sole-change]                   ::  command line edit       '
'       input events │            │ output effects              [%ret ~]                             ::  submit and clear        '
'              %belt │            │ %blit                       [%clr ~]                             ::  exit context            '
'                ┌───▼────────────┴───┐                         [%tab pos=@ud]                       ::  tab complete            '
'                │                    │                     ==                                       ::                          '
'                │   kernel (dill)    │                   ==                                         ::                          '
'                │                    │                 +$  sole-change                              ::  network change          '
'                └───┬────────────▲───┘                   $:  ler=sole-clock                         ::  destination clock       '
'        input pokes │            │ output facts              haw=@uvH                               ::  source hash             '
'         %dill-belt │            │ %dill-blit                ted=sole-edit                          ::  state change            '
'                ┌───▼────────────┴───┐                   ==                                         ::                          '
'                │                    │                 +$  sole-clock  [own=@ud his=@ud]            ::  vector clock            '
'                │  userspace (drum)  │                 +$  sole-edit                                ::  shared state change     '
'                │                    │                   $%  [%del p=@ud]                           ::  delete one at           '
'                └───┬────────────▲───┘                       [%ins p=@ud q=@c]                      ::  insert at               '
~[`'        input pokes' ' │            │ output facts              [%mor p=(list sole-edit)]              ::  combination             ']
~[`'       %sole-action' ' │            │                           [%nop ~]                               ::  no-op                   ']
'                ┌───▼────────────┴───┐                       [%set p=(list @c)]                     ::  discontinuity           '
'                │                    │                   ==                                         ::                          '
'                │    other agents    │                                                                                          '
'                │                    │                                                                                          '
'                └────────────────────┘                                                                                          '
'                                                                                                                                '
==
=;  l=(list $@(@t (list $@(@t [~ @t]))))  (turn l make)
:~
'                                                                                                                                '
'                ┌────────────────────┐                 %watch /drum/[~client-ship]/[session-id]                                 '
'                │                    │                                                                                          '
'                │  runtime (term.c)  │                 +$  sole-effect                              ::  app to sole             '
'                │                    │                   $%  [%bel ~]                               ::  beep                    '
'                └───┬────────────▲───┘                       [%blk p=@ud q=@c]                      ::  blink+match char at     '
'       input events │            │ output effects            [%bye ~]                               ::  close session           '
'              %belt │            │ %blit                     [%clr ~]                               ::  clear screen            '
'                ┌───▼────────────┴───┐                       [%det sole-change]                     ::  edit command            '
'                │                    │                       [%err p=@ud]                           ::  error point             '
'                │   kernel (dill)    │                       [%klr p=styx]                          ::  styled text line        '
'                │                    │                       [%mor p=(list sole-effect)]            ::  multiple effects        '
'                └───┬────────────▲───┘                       [%nex ~]                               ::  save clear command      '
'        input pokes │            │ output facts              [%pro sole-prompt]                     ::  set prompt              '
'         %dill-belt │            │ %dill-blit                [%sag p=path q=*]                      ::  save to jamfile         '
'                ┌───▼────────────┴───┐                       [%sav p=path q=@]                      ::  save to file            '
'                │                    │                       [%tab p=(list [=cord =tank])]          ::  tab-complete list       '
'                │  userspace (drum)  │                       [%tan p=(list tank)]                   ::  classic tank            '
'                │                    │                       [%txt p=tape]                          ::  text line               '
'                └───┬────────────▲───┘                       [%url p=@t]                            ::  activate url            '
~['        input pokes │            │ ' `'output facts          ==                                         ::                          ']
~['       %sole-action │            │ ' `'%sole-effect                                                                                 ']
'                ┌───▼────────────┴───┐                                                                                          '
'                │                    │                                                                                          '
'                │    other agents    │                                                                                          '
'                │                    │                                                                                          '
'                └────────────────────┘                                                                                          '
'                                                                                                                                '
==
=;  l=(list $@(@t (list $@(@t [~ @t]))))  (turn l make)
:~
'                                                                                                                                '
'                ┌────────────────────┐                                                                                          '
'                │                    │                                                                                          '
'                │  runtime (term.c)  │                                                                                          '
'                │                    │                                                                                          '
'                └───┬────────────▲───┘                                                                                          '
'       input events │            │ output effects                                                                               '
'              %belt │            │ %blit                                                                                        '
'                ┌───▼────────────┴───┐                                                                                          '
'                │                    │                                                                                          '
'                │   kernel (dill)    │                                                                                          '
'                │                    │                                                                                          '
'                └───┬────────────▲───┘                                                                                          '
'        input pokes │            │ output facts                                                                                 '
'         %dill-belt │            │ %dill-blit                                                                                   '
'                ┌───▼────────────┴───┐                                                                                          '
'                │                    │                                                                                          '
~['                │  ' `'userspace (book)' '  │                                                                                          ']
'                │                    │                                                                                          '
'                └───┬────────────▲───┘                                                                                          '
~[`'        input pokes │            │ output facts                                                                                 ']
~[`'         %dill-belt │            │ %dill-blit                                                                                   ']
'                ┌───▼────────────┴───┐                                                                                          '
'                │                    │                                                                                          '
'                │    other agents    │                                                                                          '
'                │                    │                                                                                          '
'                └────────────────────┘                                                                                          '
'                                                                                                                                '
==
=;  l=(list $@(@t (list $@(@t [~ @t]))))  (turn l make)
:~
'                                                                                                                                '
'                ┌────────────────────┐                                                                                          '
'                │                    │                                                                                          '
'                │  runtime (term.c)  │                                                                                          '
'                │                    │                                                                                          '
'                └───┬────────────▲───┘                                                                                          '
'       input events │            │ output effects                                                                               '
'              %belt │            │ %blit                                                                                        '
'                ┌───▼────────────┴───┐                                                                                          '
'                │                    │                                                                                          '
'                │   kernel (dill)    │                                                                                          '
'                │                    │                                                                                          '
'                └───┬────────────▲───┘                                                                                          '
'        input pokes │            │ output facts                                                                                 '
'         %dill-belt │            │ %dill-blit                                                                                   '
'                ┌───▼────────────┴───┐                                                                                          '
'                │                    │                                                                                          '
'                │  userspace (book)  │                                                                                          '
'                │                    │                                                                                          '
'                └───┬────────────▲───┘                                                                                          '
'        input pokes │            │ output facts                                                                                 '
~[`'%specialized-input? │            │ %ui-element?                                                                                 ']
'                ┌───▼────────────┴───┐                                                                                          '
'                │                    │                                                                                          '
'                │    other agents    │                                                                                          '
'                │                    │                                                                                          '
'                └────────────────────┘                                                                                          '
'                                                                                                                                '
==
=;  l=(list $@(@t (list $@(@t [~ @t]))))  (turn l make)
:~
'                                                                                                                                '
'                                                                                                                                '
~[`'                                                     terminal stack architecture                                                ']
'                                                                                                                                '
'                                                           further reading                                                      '
'                                                                                                                                '
'                                      code on urbit/urbit branch: wip/tui-toys                                                  '
'                                      https://github.com/urbit/urbit/tree/wip/tui-toys/pkg/demo                                 '
'                                                                                                                                '
'                                      pr #4463: first phase, "un-prompt"                                                        '
'                                      https://github.com/urbit/urbit/pull/4463                                                  '
'                                                                                                                                '
'                                      pr #5663: second phase, "session support"                                                 '
'                                      https://github.com/urbit/urbit/pull/4463                                                  '
'                                                                                                                                '
'                                      terminal exposition on urbit-dev mailing list                                             '
'                                      https://groups.google.com/a/urbit.org/g/dev/c/wydG30BgAzE/m/sRi9jeDECgAJ                  '
'                                                                                                                                '
==
::
  ==
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
    ::  (gth a l)
    ::  uh oh, stack alert!
    =/  r  $(a (sub a l), b t.b)
    [[i.b p.r] q.r]
  --
::
--
