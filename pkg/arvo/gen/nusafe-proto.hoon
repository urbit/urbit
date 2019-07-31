/-  ring, *safe-applet, common=safe-common, client=safe-client, server=safe-server
/+  ring, *safe-signatures, *safe-client, *safe-common, *safe-server, safe-applets
::
::  To post to /board/1234, you must post:
::
::  [outer-sig [/board/1234 [signature %post blah]]]
::
::  In turn, the processing goes like this:
::
::  - Checks outer-signature.
::
::  - Calls (on-route-for-child:toplevel outer-sig ~ /), which can kill the
::  processing or can send a piece of data downwards
::
::  - Calls (on-route-for-child:board outer-sig ~ /board), which can kill or send
::  a piece of data downwards. In this case, a new post number is allocated and
::  is sent downwards between nodes...
::
::  - Calls (on-incoming-event:post outer-sig [~ postid=523] /board/1234
::  signature [%post blah]), which can cancel the event or can add its own
::  private data. In this case, it adds the postid allocated by its parent and
::  then 

:-  %say
|=  $:  {now/@da eny/@uvJ bec/beak}
        ~
        ~
    ==
:-  %noun
::
::  from outside, we receive [first-parent /board/123 user-event]

::  (on-route:/ ring-signature)
::  (on-route:/board <result above>)
::  (on-route:/board/123 <result above> user-event)

::
::  the event flow is:
::
::  - parent-event is either received from outside or the direct parent node.
::    - first-parent is sent to (on-route / first-parent)
::    - then (on-route /board <return value above>)
::    - then (on-process-event /board/thread <return value above> user-event)
::
::  This works for the thread (sorta) but where does spawning behaviour come in?
::


::  toplevel container node
::
=/  toplevel
  %-  instantiate-node
    :*  safe-applets
        /
        %auth
        %community
        :*  ~
            (sy [~littel-ponnys ~rovnys-ricfer ~palfun-foslup ~rapfyr-diglyt ~])
            'our town'
            ~zod
    ==  ==
::

::
::  initializes the 'our town' community
::
~&  %server---------1
=^  ret1  toplevel
  (apply-to-server safe-applets [[%ship ~zod 5] [%ship ~zod 5] / [%invite ~ponnys-podfer]] toplevel)
~&  [%changes ret1]
::  'our town' should have a 'shitposting' board
::
~&  %server---------2
=^  ret2  toplevel
  (apply-to-server safe-applets [[%ship ~zod 5] [%ship ~zod 5] / [%create 'shitposting' %board %unlinked]] toplevel)
~&  [%changes ret2]
::  time to start shitposting!
::
~&  %server---------3
=^  ret3  toplevel
  (apply-to-server safe-applets [[%ship ~zod 5] [%ship ~zod 5] /shitposting [%new-post 'subject' 'text']] toplevel)
~&  [%changes ret3]

~&  %client---------1
::  The client wants to post to /shitposting/1, and to do so, it needs the
::  information about /, /shitposting, and /shitposting/1.
::
=|  client-state=node:client
::
=/  root-state=(unit peer-diff:common)  (get-snapshot-as-peer-diff / toplevel)
=.  client-state  (apply-to-client safe-applets [/ (need root-state)] client-state)
::
=/  board-state=(unit peer-diff:common)  (get-snapshot-as-peer-diff /shitposting toplevel)
=.  client-state  (apply-to-client safe-applets [/shitposting (need board-state)] client-state)
::
=/  snapshot-state=(unit peer-diff:common)  (get-snapshot-as-peer-diff /shitposting/1 toplevel)
=.  client-state  (apply-to-client safe-applets [/shitposting/1 (need snapshot-state)] client-state)

~&  [%server-state toplevel]
~&  [%client-state client-state]

::  continue shitposting in the current thread!
::
~&  %server---------4
=^  ret4  toplevel
  (apply-to-server safe-applets [[%ship ~zod 5] [%ship ~zod 5] /shitposting/1 [%new-post 'reply' 'text reply']] toplevel)
~&  [%changes ret4]

?>  ?=(^ ret4)
=.  client-state  (apply-to-client safe-applets [/shitposting/1 peer-diff.i.ret4] client-state)


~&  %client---------2

::  Here's a signed request from the client to be sent into the server.
::
~&  [%signed-request (sign-user-event ~zod now eny /shitposting/1 [%new-post 'another' 'more'] client-state safe-applets)]

::  zero
::
0


:: The system verifies that the signatures are valid before dispatch, but the
:: signatures are made available so that the system can perform its own
:: additional checks.
