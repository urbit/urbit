::
::::
  ::
/-  *story
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [[syd=desk ~] ~]
    ==
|^
=/  our  p.bec
=/  cas  `case`[%da now]
=/  tale  .^(story %cx /(scot %p our)/[syd]/(scot %da now)/story)
=/  current-tako  .^(tako:clay %cs /(scot %p our)/[syd]/(scot cas)/tako/~)
=/  current-yaki  .^(yaki:clay %cs /(scot %p our)/base/(scot cas)/yaki/(scot %uv current-tako))
:-  %tang
(story-log current-yaki tale)
::
++  msg-from-commit
  |=  [commit=yaki:clay tale=story]
  ^-  (unit cord)
  =/  proses  (~(get by tale) r.commit)
  ?~  proses  ~
  =/  prose-list  ~(tap in u.proses)
  ::  TODO signal conflict
  =/  prose-first  (snag 0 prose-list)
  =/  title  title:prose-first
  =/  body   body:prose-first
  %-  some
  (crip "commit: {<`@uv`r.commit>}\0atitle: {<title>}\0abody:\0a{<body>}")
::::
::  Remarks:
::
::  There are two recursions in the log file:
::  1. the outer loop, `commit-loop` which threads downwards into each commit by ancestor
::  2. the inner loop, `ancestor-loop`, which threads left-to-right on reverse-ancestors
::::
++  story-log
  |=  [current-commit=yaki:clay tale=story]
  ^-  tang
  =/  our  p.bec
  =/  cas=case  [%da now]
  %-  flop
  %-  head
  =|  state=[result=tang mergebase=(unit tako:clay)]
  |-
  ^-  _state
  =*  commit-loop  $
  =/  reverse-ancestors  (flop p.current-commit)
  |-
  =*  ancestor-loop  $
  ?-    reverse-ancestors
      ~
    :: stop here and return the current message
    =/  msg=(unit cord)  (msg-from-commit current-commit tale)
    [?~(msg result.state [u.msg result.state]) mergebase=~]
  ::
      [tako:clay ~]
    =/  parent=tako:clay  i.reverse-ancestors
    =/  parent-commit     .^(yaki:clay %cs /(scot %p our)/base/(scot cas)/yaki/(scot %uv parent))
    =/  msg  (msg-from-commit current-commit tale)
    :: proper logic is: if there is a mergebase and the current commit is the mergebase, stop. otherwise, continue 
    ?:  ?&(?=(^ mergebase.state) =(u.mergebase.state r.current-commit))
      :: stop here and clear mergebase. we skip adding the mergebase's msg itself because it will be added through the other branch
      [result=result.state mergebase=~]
    commit-loop(current-commit parent-commit, state [?~(msg result.state [u.msg result.state]) mergebase.state])
  ::
      [tako:clay tako:clay ~]
    ::
    ::  mainline: ultimate base chain
    ::  nextline: relative mainline
    ::  sideline: side-chain, featurebranch
    ::
    ::  From the context of e, commit c is on its relative mainline, or nowline, while commit d is on its sideline
    ::  %base  a--b-------------X  :: mainline
    ::  %new      \--c------e--/   :: nowline
    ::  %new2        \--d--/       :: sideline 
    ::
    ::  
    ~&  "in merge commit"
    =/  sideline=tako:clay               i.reverse-ancestors
    =/  mainline=tako:clay               i.t.reverse-ancestors
    =/  mergebases                       .^((list tako:clay) %cs /(scot %p our)/base/(scot cas)/base-tako/(scot %uv mainline)/(scot %uv sideline))
    =/  next-mergebase=(unit tako:clay)  ?~(mergebases ~ (some i.mergebases))  :: take the first valid mergebase (by convention) if exists, else none
    =/  sideline-commit=yaki:clay        .^(yaki:clay %cs /(scot %p our)/base/(scot cas)/yaki/(scot %uv sideline))
    =/  mainline-commit=yaki:clay        .^(yaki:clay %cs /(scot %p our)/base/(scot cas)/yaki/(scot %uv mainline))
    =/  msg=(unit cord)                  (msg-from-commit current-commit tale)
    ::
    ::  1 - process current commit
    ::  2 - recur and queue processing on all commits on the sideline
    ::  3 - recur and queue processing on all commits on the mainline
    ::
    ::  Because mainline messages are cons'd to result last, they are 
    ::  (by definition) towards the less recent side of the (resulting, flopped) list
    ::
    =.  state  [result=?~(msg result.state [u.msg result.state]) mergebase=next-mergebase]  :: 1
    =.  state  commit-loop(current-commit sideline-commit)                                  :: 2
    =.  state  commit-loop(current-commit mainline-commit)                                  :: 3
    state
  ::
      [tako:clay tako:clay tako:clay *]
    ~&  "in 3+ ancestor commit"
    :: take the first two (sideline/mainline). calculate that mergebase
    :: recur through sideline (state-b). but instead of state-c line for mainline,
    =/  sideline=tako:clay               i.reverse-ancestors
    =/  nowline=tako:clay                i.t.reverse-ancestors
    =/  mergebases                       .^((list tako:clay) %cs /(scot %p our)/base/(scot cas)/base-tako/(scot %uv nowline)/(scot %uv sideline))
    =/  next-mergebase=(unit tako:clay)  ?~(mergebases ~ (some i.mergebases))  :: take the first valid mergebase (by convention) if exists, else none
    =/  sideline-commit=yaki:clay        .^(yaki:clay %cs /(scot %p our)/base/(scot cas)/yaki/(scot %uv sideline))
    =.  mergebase.state  next-mergebase
    =.  state            commit-loop(current-commit sideline-commit)
    =.  state            ancestor-loop(reverse-ancestors t.reverse-ancestors)
    state
  ==
--