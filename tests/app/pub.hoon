::  pub
::
/+  *server, default-agent, verb, dbug
=,  format
::
|%
::
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
+$  state-0  [%0 ~]
+$  action
  $%  [%send data=@]
      [%bye who=ship]
      [%flus data=@]
      [%flas who=ship]
      [%null ~]
      [%hola who=ship]
      [%germ =coop:gall]
      [%tend =coop:gall =path =page]
  ==
--
%+  verb  |
%-  agent:dbug
=|  state-0
=*  state  -
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  on-init:def
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 old))]
::
++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?.  ?=(%noun mark)
      (on-poke:def mark vase)
    =+  action=;;(action !<(* vase))
    ~&  >  %out
    :_  this
    ~&  >>  -.action
    ?-  -.action
      %send  ~&(%sending [%give %fact [/subs]~ atom+!>(data.action)]~)
      %bye   ~&(%kicking [%give %kick [/subs]~ `who.action]~)
      %flus  ~&(%flushing [%give %fact [/flus]~ atom+!>(data.action)]~)
      %flas  ~&(%flashing [%give %kick [/flus]~ `who.action]~)
      %null  ~&(%null !!)
      %germ  [%pass /my-wire %germ coop.action]~
      %tend  [%pass /my-wire %tend coop.action path.action page.action]~
      ::
        %hola
      =/  tid         `@ta`(cat 3 'thread_' (scot %uv (sham eny.bowl)))
      =/  ta-now      `@ta`(scot %da now.bowl)
      =/  =beak       byk.bowl(r da+now.bowl)
      =/  start-args  [~ `tid beak %hola !>([~ who.action "hola"])]
      =/  =cage       spider-start+!>(start-args)
      =/  spider      [our.bowl %spider]
      =/  =wire       /thread-result/[tid]
      ~&  tid+tid
      :~  [%pass /hola/[ta-now] %agent spider %watch wire]
          [%pass /hola/[ta-now] %agent spider %poke cage]
      ==
    ==
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  :_  this
  ?+  path  (on-watch:def path)
    :: XX allow users for crashes here to trigger naxplanations
    ::
      [%subs ~]  ~&  subs+src.bowl  ~ :: (on-watch:def path)  
      [%sabs ~]  ~&  sabs+src.bowl  ~ :: (on-watch:def path)
      [%sups ~]  ~&  sups+src.bowl
    :~  [%give %fact ~ atom+!>(%done)]
        [%give %kick ~ ~]
    ==
  ::
      [%flus ~]  ~&  sabs+src.bowl  ~
  ==
::
++  on-leave
  |=  =path
  ^-  (quip card _this)
  ~&  leave+[src.bowl path]
  :: XX allow users for crashes here to trigger naxplanations
  ::
  ?:  &  !!
  `this
  :: ?.  ?=([%subs ~] path)  `this
  :: :_  this
  :: [%give %kick ~[/sups] `src.bowl]~
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ~&  >>  on-peek/path
  ?.  ?=([%c %my %context @ ~] path)
    ``[%noun !>(%.n)]
  :: =/  =ship  (slav %p i.t.t.t.t.path)
  ``[%noun !>(%.y)]
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ~&  wire+wire
  ?+  wire  (on-agent:def wire sign)
    [%hola *]  ~&  hi+-.sign  [~ this]
  ==
::
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def

--
