::  story: log commits in order
::
::::
  ::
/-  *story
/+  lib=story
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [[~] =desk ~]
    ==
|^
=/  our                   p.bec
=?  desk  =(*^desk desk)  q.bec  :: use current desk if user didn't provide
=/  cas                   r.bec  :: use case from beak
=/  pax                   /(scot %p our)/[desk]/(scot cas)/story
?:  !(~(has in .^((set ^desk) %cd /(scot %p our)/$/(scot %da now))) desk)
  tang+[leaf+"Error: desk {<desk>} does not exist." ~]
?:  !.^(? %cu pax)
  tang+['Error: No story file found. Please use |story-init to create one.' ~]
=/  tak   .^(tako:clay %cs /(scot %p our)/[desk]/(scot cas)/tako/~)
=/  yak   .^(yaki:clay %cs /(scot %p our)/[desk]/(scot cas)/yaki/(scot %uv tak))
=/  tale  .^(story %cx pax)
:-  %tang
(story-read [our desk cas] yak tale)
::::
::  Remarks:
::
::  There are two recursions in the logging process:
::  1. the outer loop `commit-loop` threads down into each commit by ancestor
::  2. the inner loop `ancestor-loop` threads left-to-right on reverse-ancestors
::
::  +story-read outputs a tang with the least-recent commits at the head
::  of the list, even though we want most-recent commits to print first.
::  But because dojo prints tangs in reverse, we don't flop the results.
::::
++  story-read
  |=  [[our=ship syd=^desk cas=case] this-commit=yaki:clay tale=story]
  ^-  tang
  ::  TODO factor out /(scot %p our)/[syd]/(scot cas)
  %-  head  :: result from state
  =|  state=[result=tang mergebase=(unit tako:clay)]
  |-
  ^-  _state
  =*  commit-loop  $
  =/  reverse-ancestors  (flop p.this-commit)
  |-
  =*  ancestor-loop  $
  ?-    reverse-ancestors
      ~
    ::  stop here and return the current message
    =/  msg=wain  (msg-from-commit this-commit tale)
    [(weld msg result.state) mergebase=~]
  ::
      [tako:clay ~]
    =/  parent  i.reverse-ancestors
    =/  parent-commit
      .^(yaki:clay %cs /(scot %p our)/[syd]/(scot cas)/yaki/(scot %uv parent))
    ::
    =/  msg
      (msg-from-commit this-commit tale)
    ::
    ::  If there is a mergebase and we are visting it right now:
    ::    stop here and clear the mergebase.
    ::    skip adding the mergebase's msg itself,
    ::    because it will be added through the other branch.
    ::  Otherwise, record the current message if exists and recur.
    ?:  ?&(?=(^ mergebase.state) =(u.mergebase.state r.this-commit))
      [result=result.state mergebase=~]
    commit-loop(this-commit parent-commit, result.state (weld msg result.state))
  ::
      [tako:clay tako:clay ~]
    ::
    ::  mainline: ultimate base chain
    ::  nowline: relative mainline
    ::  sideline: side-chain, featurebranch
    ::
    ::  From the context of e, commit c is on its relative mainline, or nowline,
    ::  while commit d is on its sideline.
    ::
    ::  %base  a--b-------------X  :: mainline
    ::  %new      \--c------e--/   :: nowline
    ::  %new2        \--d--/       :: sideline
    ::
    ::
    =/  sideline         i.reverse-ancestors
    =/  mainline         i.t.reverse-ancestors
    ::  XX base-tako ignores beak
    ::
    =/  mergebases
      .^  (list tako:clay)  %cs
          (scot %p our)  syd  (scot cas)
          /base-tako/(scot %uv mainline)/(scot %uv sideline)
      ==
    ::
    ::  Take the first valid mergebase (by convention) if exists, else none
    ::
    =/  next-mergebase
      ?~(mergebases ~ (some i.mergebases))
    ::
    =/  sideline-commit
      .^(yaki:clay %cs /(scot %p our)/[syd]/(scot cas)/yaki/(scot %uv sideline))
    ::
    =/  mainline-commit
      .^(yaki:clay %cs /(scot %p our)/[syd]/(scot cas)/yaki/(scot %uv mainline))
    ::
    =/  msg=wain  (msg-from-commit this-commit tale)
    ::
    ::  1 - process current commit
    ::  2 - recur and queue processing on all commits on the sideline
    ::  3 - recur and queue processing on all commits on the mainline
    ::
    ::  Because mainline messages are cons'd to result last, they are
    ::  (by definition) towards the less recent side of the flopped list
    ::
    =.  state  [result=(weld msg result.state) mergebase=next-mergebase]  :: 1
    =.  state  commit-loop(this-commit sideline-commit)                   :: 2
    =.  state  commit-loop(this-commit mainline-commit)                   :: 3
    state
  ::
      [tako:clay tako:clay tako:clay *]
    ::  ~&  "in 3+ ancestor commit"
    =/  sideline    i.reverse-ancestors
    =/  nowline     i.t.reverse-ancestors
    =/  mergebases
      .^  (list tako:clay)  %cs
          (scot %p our)  syd  (scot cas)
          /base-tako/(scot %uv nowline)/(scot %uv sideline)
      ==
    ::
    ::  Take the first valid mergebase (by convention) if exists, else none
    ::
    =/  next-mergebase   ?~(mergebases ~ (some i.mergebases))
    =/  sideline-commit
      .^(yaki:clay %cs /(scot %p our)/[syd]/(scot cas)/yaki/(scot %uv sideline))
    =.  mergebase.state  next-mergebase
    =.  state  commit-loop(this-commit sideline-commit)             :: downward
    =.  state  ancestor-loop(reverse-ancestors t.reverse-ancestors) :: rightward
    state
  ==
::
++  msg-from-commit
  |=  [commit=yaki:clay tale=story]
  ^-  wain
  =/  proses  (~(get ju tale) r.commit)
  ?~  proses  ~
  %-  flop    :: fixes formatting reversal in dojo
  (chapter-to-text:lib r.commit proses)
--
