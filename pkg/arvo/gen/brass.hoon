::  Produce a brass pill
::
::::  /hoon/brass/gen
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
        prime=_|
        exc=(list spur)
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
=/  dez=(list [desk path])
  ?~  arg  ~
  %+  turn  rest.arg
  |=  =desk
  [desk /(scot %p p.bec)/[desk]/(scot %da now)]
::
(brass:pill sys dez prime exc)
