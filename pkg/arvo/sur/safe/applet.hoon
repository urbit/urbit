/-  ring
::  Types used in the implementation of a safe applet
::
|%
::  You can use any applet as your toplevel as long as it obeys this toplevel
::  interface. The toplevel-interface is how Safe calls the configured toplevel
::  node.
::
++  toplevel-interface
  |%
  ::  the input to your node; this is your parent-event
  ::
  ++  input
    full-signature
  ::  the output of your node; this is your return-event
  ::
  ++  output
    $%  ::  accepts the message and release all the effects and state changes
        ::  it produced.
        ::
        [%accept ~]
        ::  rejects this message and ignores all the effects and reverts all
        ::  state changes.
        ::
        [%reject ~]
        ::  a special output that can only occur on the toplevel node; this
        ::  accepts the incoming new invitees.
        ::
        [%accept-and-invite-member ship=@p]
        ::
        ::  TODO: Should there be an uninvite here? If there should be, how do
        ::  you avoid abusive @p posting? If not, how do you fix threats to
        ::  eject yourself in response to blackmail?
    ==
  --
::  The toplevel signature is what comes in on the auth app.
::
++  full-signature
  $%  [%ship ship=@p sig=@]
      [%ring =ring-signature:ring]
  ==
::  A processed signature is an attestation by a parent node to a child node
::  that a valid signature was made.
::
++  processed-signature
  $%  [%ship ship=@]
      [%ring =path tag=(unit @udpoint)]
  ==
::
++  process-signature
  |=  [=path =full-signature]
  ^-  processed-signature
  ::
  ?-  -.full-signature
    %ship  [%ship ship.full-signature]
    %ring  [%ring path y.raw.ring-signature.full-signature]
  ==
::  The +on-process-event arm in your applet will return one of these to direct
::  what the next processing action is.
::
+$  on-process-response
  $%  ::  emits an event to this node's event log with a corresponding piece of data
      ::
      [%log private-event=vase return-event=vase]
      ::  creates a new node and re-dispatch the event to it
      ::
      [%create sub-id=@t app-type=@t =signature-type child-event=vase]
      ::  returns a value upwards
      ::
      [%return return-event=vase]
  ==
::  what sort of signature this node wants
::
::    We don't want to give applets the ability to build arbitrary signatures,
::    since they could leak information that way. Instead, we make the sort of
::    signature a node wants
::
+$  signature-type
  $?  ::  sign publicly with your ship identity
      ::
      %ship
      ::  sign with an unlinked ring-signature.
      ::
      %unlinked
      ::  sign with a linked ring-signature with scope of the community.
      ::
      %community
      ::  sign with a linked ring-signature with scope of the current path.
      ::
      %self
      ::  walk upwards recursively to find a strategy
      ::
      %inherit
  ==
--
