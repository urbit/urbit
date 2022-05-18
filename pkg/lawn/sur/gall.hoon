|%
+$  bowl
  $:  $:  our=ship
          src=[=ship =desk]
          wer=path  ::  fully qualified path to agent /ship/desk/agent
      ==
      $:  act=@ud
          eny=@
          now=@da
      ==
      $:  pem=(set perm)  ::  all permissions this agent has
  ==  ==
::  $pub-meta: metadata stored with each publication
::
+$  pub-meta
  $:  rev=@ud  ::  latest revision
      chk=@ud  ::  revision of latest checkpoint
      int=@ud  ::  number of updates between checkpoints
  ==
::  $sub-meta: metadata stored with each subscription
::
+$  sub-meta
  $:  live=_|   ::  has the publisher acked this request?
      sent=@da  ::  when did we first subscribe?
      pub-meta  ::  publication metadata
  ==
::
++  agent
  |%
  ::  $vase-note: $note subtypes that would include vases if universal
  ::
  ::    Probably just for documentation, unless Gall wants to use this
  ::    as an implementation detail of move type checking.
  ::
  +$  vase-note
    $%  [%poke =ship =path =vase]
        [%wave =ship =cage]
    ==
  ::  $move: any effect from an agent
  +$  move
    $%  [%pass =wire =note]
        ::[%give =gift]  ::  why are there no gifts?
    ==
  ::  $note: request emitted by agent
  ::
  +$  note
    ::  to userspace
    ::
    $%  [%gaze =ship =path]  ::  subscribe
        [%crag =mark =path]  ::  initialize publication
        [%fend =path]        ::  set permissions
    ::  to the kernel
    ::
    ::  %ames
        [%sift ships=(list ship)]
        [%spew veb=(list verb)]
    ::  %behn
        [%wait date=@da]
        [%rest date=@da] 
    ::  %clay 
        [%cred nom=@ta cew=crew]
        [%info des=desk dit=(axal soya)]  ::  NOTE page not cage
        [%merg des=desk her=@p dem=desk cas=case how=germ]
        [%fuse des=desk bas=beak con=(list [beak germ])]
        [%mont pot=term bem=beam]
        [%ogre pot=$@(desk beam)]
        [%dirk =desk]
        [%perm des=desk pax=path rit=rite]
        [%warp wer=ship rif=riff]
    ::  %dill  ::  TODO ask ~palfun-foslup for review
        [%belt =belt]
        [%flog =flog]
        [%crud =goof]
        [%view ~]
    ::  %eyre
        [%rule =http-rule]
        [%connect =binding]
        [%disconnect =binding]
        [%approve-origin =origin]
        [%reject-origin =origin]
    ::  %gall
        [%jolt =desk peer=[=ship =desk]]
        [%idle =desk]
        [%nuke =desk]
    ::  %iris
        [%request =request:http =outbound-config]
        [%cancel-request ~]
    ::  %jael
        [%snag ~]  ::  ask jael to track pki from this desk
        [%listen whos=(set ship) =source]
        [%nuke whos=(set ship)]
        [%moon =ship =udiff:point]
        [%public-keys ships=(set ship)]
        [%rekey =life =ring]
        [%ruin ships=(set ship)]
        [%step ~]
    ==
  +$  perm
    $%  perm-arvo
        [%gent perm-gent]
    ==
  +$  perm-gent
    $%  [%call-local ?]  ::  can it poke and subscribe to local desks?
        [%call-peers ?]  ::  can it poke and subscribe to foreign ships?
        [%berm-local ?]  ::  can local desks poke and subscribe to it?
        [%berm-peers ?]  ::  can foreign ships poke and subscribe to it?
    ==
  +$  perm-arvo
    $%  [%ames ?(%debug)]  ::  %sift %spew
        [%behn ?(%timer)]  ::  %wait %rest
        ::  %mount: %mont %ogre %dirk
        ::  %write: %info %merg %fuse
        ::  %permissions: %cred %perm
        ::  %query-local: %warp
        ::  %query-peers: %werp
        ::  %build: %warp with a ford build request $care
        ::
        [%clay ?(%mount %write %permissions %query-local %query-peers %build)]
        ::  %trace: %crud
        ::  %draw: %belt %flog %view (TODO: is this right?)
        ::
        [%dill ?(%trace %draw)]
        ::  %cors: %approve-origin %reject-origin
        ::  %serve-web: %rule %connect %disconnect
        ::
        [%eyre ?(%serve-web %cors)]
        ::  %toggle: %jolt idle
        ::  %nuke: %nuke
        ::  %perm: set these permissions for agents
        ::
        [%gall ?(%toggle %perm %nuke)]
        ::  %web-request: %request %cancel-request
        ::
        [%iris ?(%request-web)]
        ::  %public-keys is always allowed, since it's public
        ::
        [%jael ?(%snag %moon %ruin %listen %step %rekey %private-keys)]
    ==
  ::  $soya: untrusted change to a file (using $page, not $cage)
  ::
  +$  soya
    $:  [%del ~]
        [%ins p=page]
        [%dif p=page]
        [%mut p=page]
    ==
  ::  $vase-sign: $sign subtypes that would include vases if universal
  ::
  +$  vase-sign
    $:  [%wave =path wave=cage rock=cage]
    ==
  +$  sign
    ::  from userspace
    ::
    $%  [%poke-ack =ship =path dud=(unit tang)]
        [%gaze-ack =ship =path dud=(unit tang)]
    ::  from the kernel
    ::  %behn
        [%wake ~]  ::  TODO error
    ::  %clay
        [%mere p=(each (set path) (pair term tang))]
        [%writ p=riot]
    ::  %dill
        [%bbye ~]
        [%blit p=(list blit)]
        [%burl p=@t]
        [%logo ~]
    ::  %eyre
        [%response =http-event:http]
        [%bound accepted=? =binding]
    ::  %iris
        [%http-response =client-response] 
    ::  %jael
        [%private-keys =life vein=(map life ring)]
        [%public-keys =public-keys-result]
    ==
  --
--
