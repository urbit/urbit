::  compose with pull-hook
::
/-  todo
/+  default-agent, verb, dbug
|%
++  poke
  $%  [%create-regular-day start=@da]
      [%shuffle ~]
  ==
--
::
%+  verb  |
%-  agent:dbug
^-  agent:gall
::
=|  todos=form:todo
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
++  on-init   on-init:def
++  on-save   !>(todos)
++  on-load   on-load:def
++  on-poke
  |=  [=mark =vase]
  ?>  ?=(%noun mark)
  =+  .^  =(says:clay form:todo diff:todo)
          %ce  /(scot %p our.bowl)/home/(scot %da now.bowl)/todo
      ==
  =|  cards=(list card:agent:gall)
  |^
  =+  ;;(=poke q.vase)
  =^  cards  todos
    ?-  -.poke
      %create-regular-day  (create-regular-day start.poke)
      %shuffle             shuffle
    ==
  [cards this]
  ::
  ++  in-this  .
  ++  create-regular-day
    |=  start=@da
    =/  random  ~(. og (shas %pubb eny.bowl))
    =^  new-id  random  (raws:random 128)
    =.  in-this  (edit-todos %upsert new-id 'slonk eggs' start)
    =^  new-id  random  (raws:random 128)
    =.  in-this  (edit-todos %upsert new-id 'run' (add start ~m30))
    =^  new-id  random  (raws:random 128)
    =.  in-this  (edit-todos %upsert new-id 'work' (add start ~h2))
    =^  new-id  random  (raws:random 128)
    =.  in-this  (edit-todos %upsert new-id 'lift' (add start ~h14))
    =^  new-id  random  (raws:random 128)
    =.  in-this  (edit-todos %upsert new-id 'sleep' (add start ~h16))
    [(flop cards) todos]
  ::
  ++  shuffle
    =.  in-this  (edit-todos %relist (flop order.todos))
    [(flop cards) todos]
  ::
  ++  edit-todos
    |=  =diff:todo
    ^+  in-this
    =:  todos  (~(pact says todos) diff)
        cards  :_(cards [%give %fact ~[/todos] %todo-diff !>(diff)])
      ==
    in-this
  --
::
++  on-watch
  |=  =path
  ?.  =(/todos path)
    (on-watch:def path)
  [[%give %fact ~ %todo !>(todos)]~ this]
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
