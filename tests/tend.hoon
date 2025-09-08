/+  verb, default-agent, dbug
|%
+$  state-0  [%0 ~]
+$  card  card:agent:gall
+$  coop  coop:gall
+$  action
  $%  [%tend =coop =path =page]
      [%germ =coop]
      [%snip =coop]
      [%keen case=@ud =path]
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
      %+  welp  /g/x/(scot %ud case.action)
     path.action
    [%pass /keen %keen & ?:(=(our.bowl ~met) ~hex ~met) path]~
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
  ~&  syn
  ?:  =(/keen wire)
    ?:  ?=([%ames %sage *] syn)
      ?~  q.sage.syn
        ~&  no-item/sage.syn
        `this
      ~&  ;;([@tas @tas] q.sage.syn)
      `this
    `this
  `this
::
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-fail   on-fail:def
--
