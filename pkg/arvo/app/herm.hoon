::  herm: stand-in for term.c with http interface
::
/+  default-agent, dbug, verb
=,  jael
|%
+$  state-0  [%0 ~]
--
::
=|  state-0
=*  state  -
%+  verb  |
%-  agent:dbug
^-  agent:gall
=>  |%
    ++  request-tube
      |=  [bowl:gall from=mark to=mark next=?]
      ^-  card:agent:gall
      :*  %pass  /tube/[from]/[to]
          %arvo  %c     %warp
          our    q.byk  ~
        ::
          ?:  next
            [%next %c da+now /[from]/[to]]
          [%sing %c da+now /[from]/[to]]
      ==
    --
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card:agent:gall _this)
  :_  this
  ::  set up dill session subscription,
  ::  and ensure the tubes we use are in cache
  ::
  :~  [%pass [%view %$ ~] %arvo %d %view ~]
      (request-tube bowl %blit %json |)
      (request-tube bowl %json %belt |)
  ==
::
++  on-save   !>([%0 ~])
++  on-load
  |=  old=vase
  ^-  (quip card:agent:gall _this)
  [~ this(state [%0 ~])]
::
++  on-watch
  |=  =path
  ^-  (quip card:agent:gall _this)
  ?>  ?=([%session @ ~] path)
  :_  this
  ::  scry prompt and cursor position out of dill for initial response
  ::
  =/  base=^path
    /dx/(scot %p our.bowl)//(scot %da now.bowl)/sessions
  :~  [%give %fact ~ %blit !>(.^(blit:dill (weld base //line)))]
      [%give %fact ~ %blit !>(`blit:dill`hop+.^(@ud (weld base //cursor)))]
  ==
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card:agent:gall _this)
  ?+  wire  !!
    ::  pass on dill blits for the session
    ::
      [%view %$ ~]
    ?.  ?=([%dill %blit *] sign-arvo)
      ~|  [%unexpected-sign [- +<]:sign-arvo]
      !!
    :_  this
    %+  turn  p.sign-arvo
    |=  =blit:dill
    [%give %fact [%session %$ ~]~ %blit !>(blit)]
  ::
    ::  ensure the tubes we need remain in cache
    ::
      [%tube @ @ ~]
    =*  from  i.t.wire
    =*  to  i.t.t.wire
    ?.  ?=([%clay %writ *] sign-arvo)
      ~|  [%unexpected-sign [- +<]:sign-arvo]
      !!
    :_  this
    [(request-tube bowl from to &)]~
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card:agent:gall _this)
  ?.  ?=(%belt mark)
    ~|  [%unexpected-mark mark]
    !!
  :_  this
  [%pass [%belt %$ ~] %arvo %d %belt !<(belt:dill vase)]~
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-fail   on-fail:def
--
