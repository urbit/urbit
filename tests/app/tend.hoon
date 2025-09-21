/+  verb, default-agent, dbug
|%
+$  state-0  [%0 ~]
+$  card  card:agent:gall
+$  coop  coop:gall
+$  action
  $%  [%tend =coop =path =page]
      [%germ =coop]
      [%snip =coop]
      [%keen =ship case=@ud =path]
  ==
--
::
=|  state-0
=*  state  -
%+  verb  |
%-  agent:dbug
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card:agent:gall _this)
  [~ this]
::
++  on-save   !>([%0 ~])
++  on-load
  |=  old=vase
  ^-  (quip card:agent:gall _this)
  [~ this(state [%0 ~])]
::
++  on-poke
  |=  [=mark =vase]
  ~|  mark/mark
  ?>  =(%noun mark)
  =+  ;;(=action q.vase)
  :_  this
  ?:  ?=(%keen -.action)
    =/  =path
      %+  welp  /g/x/(scot %ud case.action)/[dap.bowl]//1
     path.action
    [%pass /keen %keen & ship.action path]~
  [%pass /foo action]~
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ~&  peek-path/path
  ~&  eny/eny.bowl
  ?.  ?=([%c *] path)
    [~ ~]
  ``noun+!>(&)
++  on-watch  on-watch:def
++  on-arvo
  |=  [=wire syn=sign-arvo]
  ^-  (quip card _this)
  ?:  =(/keen wire)
    ?:  ?=([%ames %sage *] syn)
      ?~   q.sage.syn
        ~&  no-item/sage.syn
        `this
      =/  =path
        /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/[p.q.sage.syn]
      =+  .^  =dais:clay  %cb
        path
      ==
      :_  this
      [%pass /flog %arvo %d %flog %text (noah ;;(vale.dais q.q.sage.syn))]~
    `this
  `this
::
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-fail   on-fail:def
--
