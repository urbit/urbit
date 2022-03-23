::  This agent is primarily for exposing the scry interface for doccords. This
::  is the primary endpoint in which Urbit apps should access doccords, such as
::  %dojo, %language-server, and %docs. Earth applications should use %flag-rpc
::  instead. Stack traces will probably be handled separately.
::
::  Example user stories:
::  User is using LSP, and clicks on a standard library function.
::  %language-server scries %flag and retrieves the doccords for that function.
::  Q: how does %flag know how to find that?
::
::  User enters ?? add into dojo. %dojo scries %flag and gets the docs for +add
::  and prints it to the terminal
::
::  User enters  ?? +generator into dojo. %dojo scries %flag and returns the
::  docs for that generator
::
/+  default-agent, dbug
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
::  Do I actually need to keep doccords in the state?
+$  state-0  [%0 ~]
--
::
%-  agent:dbug
=|  state-0
=*  state  -
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %.n) bowl)
++  on-init
  ::  Send subscription requests to clay for files to extract doccords from.
  ^-  (quip card _this)
  ~&  >  %flag-init
  `this
++  on-save  on-save:def
++  on-load  on-load:def
++  on-poke  on-poke:def
::  What pokes do I need?
::  Poke it with a hoon file to make it parse and extract the docs from it.
::  Ask it to print doccords to terminal with %sole-effect
::  print all docs within a file
++  on-watch  on-watch:def
::  What subscriptions do I need?
::  Send updated doccords to other agents when the filesystem changes
++  on-leave  on-leave:def
++  on-peek  on-peek:def
::  What scries do I need?
::  Ask it for the docs associated with a particular arm, type, or core.
::  Be able to ask for summaries or the full annotation.
::  Be able to ask it for the docs associated to any types utilized in a gate
::  or core.
++  on-agent  on-agent:def
::  No agents to subscribe to immediately come to mind.
::
++  on-arvo  on-arvo:def
::  What vane responses do I need?
::  Updates to the filesystem requiring re-parsing to get new doccords.
::  Clay only allows you to subscribe to one new version of each file, then it
::  kills the subscription. so we need to resubscribe once we receive an update
++  on-fail  on-fail:def
--
::  I need a location parser.
