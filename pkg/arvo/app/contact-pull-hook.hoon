/-  *resource
/+  store=contact-store, contact, default-agent, verb, dbug, pull-hook, agentio
/+  grpl=group
~%  %contact-pull-hook-top  ..part  ~
|%
+$  card  card:agent:gall
++  config
  ^-  config:pull-hook
  :*  %contact-store
      update:store
      %contact-update
      %contact-push-hook
      0  0
      %.y  :: necessary to enable p2p
  ==
--
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
%-  (agent:pull-hook config)
^-  (pull-hook:pull-hook config)
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    dep   ~(. (default:pull-hook this config) bowl)
    con   ~(. contact bowl)
    io    ~(. agentio bowl)
    grp   ~(. grpl bowl)
::
++  on-init   
  ^-  (quip card _this)
  :_  this
  (poke-self:pass:io noun+!>(%upgrade))^~
  
++  on-save   !>(~)
++  on-load   on-load:def
++  on-poke   
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?.  ?=(%noun mark)  (on-poke:def mark vase)
  :_  this
  %+  murn  ~(tap in scry-groups:grp)
  |=  rid=resource
  ?:  =(our.bowl entity.rid)  ~
  ?.  (is-managed:grp rid)    ~
  `(poke-self:pass:io pull-hook-action+!>([%add [entity .]:rid]))
::
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
++  on-agent  on-agent:def
++  on-watch  
  |=  =path
  ?.  ?=([%nacks ~] path)
    (on-watch:def path)
  ?>  (team:title [src our]:bowl)
  `this
::
++  on-leave  on-leave:def
++  resource-for-update  resource-for-update:con
++  on-pull-nack
  |=  [=resource =tang]
  ^-  (quip card _this)
  :_  this
  [%give %fact ~[/nacks] resource+!>(resource)]~
::
++  on-pull-kick  |=(=resource `/)
--
