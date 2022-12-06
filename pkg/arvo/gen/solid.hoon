::  Compile arvo as a pill noun, without compiler changes.
::  usage
::
::    .urbit/pill +solid
::
::::  /hoon/solid/gen
  ::
/?    310
/+  pill
::
::::
  !:
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
      ::
        ::  arg: desks to build pill from
        ::
        ::    list of desks. defaults to [%base]~.
        ::    the first desk in this list will become the pill's base desk.
        ::    optionally, the first desk may be replaced with a fully
        ::    qualified path to the new boot system (typically in sys).
        ::    the rest of the desks will be installed through kiln.
        ::
        $=  arg
        $@  ~
        $:  base=$@(desk [@ta @ta @ta path])
            rest=(list desk)
        ==
      ::
        dub=_|
    ==
:-  %pill
^-  pill:pill
::  sys: root path to boot system, `/~me/[desk]/now/sys`
::  bas: root path to boot system' desk
::  dez: secondary desks and their root paths
::
=/  sys=path
  ?:  ?=([^ *] arg)
    `path`base.arg
  =/  =desk
    ?~  arg  %base
    ?>(?=(@ base.arg) base.arg)
  /(scot %p p.bec)/[desk]/(scot %da now)/sys
=/  bas=path
  (scag 3 sys)
=/  dez=(list [desk path])
  ?~  arg  ~
  %+  turn  rest.arg
  |=  =desk
  [desk /(scot %p p.bec)/[desk]/(scot %da now)]
::
=/  compiler-path  (weld sys /hoon)
=/  arvo-path      (weld sys /arvo)
~&  %solid-start
=/  compiler-src  .^(@t %cx (weld compiler-path /hoon))
=/  arvo-src      .^(@t %cx (weld arvo-path /hoon))
=/  arvo-formula
  ~&  %solid-loaded
  =/  compiler-hoon  (rain compiler-path compiler-src)
  ?.  dub
    ::  compile arvo against hoon, with our current compiler
    ::
    =/  whole-hoon=hoon
      [%tsgr compiler-hoon [%tsgr [%$ 7] (rain arvo-path arvo-src)]]
    ~&  %solid-parsed
    =/  whole-formula  q:(~(mint ut %noun) %noun whole-hoon)
    ~&  %solid-arvo
    whole-formula
  ::  compile arvo against hoon, with a freshly compiled hoon (via +ride)
  ::
  ~&  %solid-parsed
  =/  compiler-formula  q:(~(mint ut %noun) %noun compiler-hoon)
  ~&  %solid-compiled
  =/  whole-src
    (rap 3 ['=>  ' compiler-src '=>  +7  ' arvo-src ~])
  ~&  %solid-double-loaded
  =/  whole-formula
    =<  +
    .*  [%noun whole-src]
    [%8 compiler-formula [%9 2 %10 [6 %0 3] [%0 2]]]
  ~&  %solid-double-compiled
  whole-formula
::
~&  [%solid-kernel `@ux`(mug arvo-formula)]
::
::  installed: Arvo gate (formal interface) with %zuse and vanes installed
::
=/  installed
  =<  q
  %^    spin
      ^-  (list ovum)
      :-  (boot-ovum:pill compiler-src arvo-src)
      %+  turn
        (snoc (turn dez tail) bas)
      file-ovum2:pill
    .*(0 arvo-formula)
  |=  [ovo=ovum ken=*]
  [~ (slum ken [now ovo])]
::
::  boot-two: startup formula
::
::    We evaluate :arvo-formula (for jet registration),
::    then ignore the result and produce .installed
::
=/  boot-two
  =>  *[arvo-formula=^ installed=^ tale=*]
  !=  =+(.*(0 arvo-formula) [installed tale])
::
::  boot-ova
::
=/  boot-ova=(list)
  [aeon:eden:part boot-two arvo-formula installed ~]
::
::  a pill is a 3-tuple of event-lists: [boot kernel userspace]
::
::    Our kernel event-list is ~, as we've already installed them.
::    Our userspace event-list is a list containing a full %clay
::    filesystem sync event.
::
:+  %pill  %solid
:+  boot-ova  ~
%+  turn
  (snoc dez [%base bas])
file-ovum:pill
