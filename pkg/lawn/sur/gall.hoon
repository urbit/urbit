|%
+$  bowl
  $:  $:  our=ship
          src=ship
          wer=path
      ==
      $:  act=@ud
          eny=@
          now=@da
  ==  ==
::
+$  pub-meta
  $:  crew=(unit (set ship))
  ==
::
+$  sub-meta
  $:  live=_|
  ==
::
++  agent
  |%
  +$  note
    ::  to userspace (cages here, but statically typed from agents)
    ::
    $%  [%poke =ship =path =cage]             ::  command
        [%gaze =ship =path]                   ::  subscribe
        [%crag =mark =path =crew]             ::  initialize publication
        [%fend =path who=(set ship) gap=@dr]  ::  set permissions
        [%wave =path =cage]                   ::  apply diff to publication
    ::  to the kernel
    ::
    ::  %ames
        [%sift ships=(list ship)]
        [%spew veb=(list verb)]
    ::  %behn
        [%wait =path date=@da]
        [%rest =path date=@da] 
    ::  %clay 
        [%cred nom=@ta cew=crew]
        [%drop des=desk]
        [%info des=desk dit=(axal soya)]  ::  NOTE page not cage
        [%merg des=desk her=@p dem=desk cas=case how=germ]
        [%fuse des=desk bas=beak con=(list [beak germ])]
        [%mont pot=term bem=beam]
        [%ogre pot=$@(desk beam)]
        [%dirk =desk]
        [%perm des=desk pax=path rit=rite]
        [%warp wer=ship rif=riff]
    ::  %dill
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
        [%full points=(map ship point)]
        [%diff who=ship =diff:point]
        [%breach who=ship]
    ==
  ::  $soya: untrusted change to a file
  ::
  +$  soya
    $:  [%del ~]
        [%ins p=page]
        [%dif p=page]
        [%mut p=page]
    ==
  +$  sign
    ::  from userspace
    ::
    $%  [%wave =path =cage]
        [%poke-ack =ship =path]
        [%gaze-ack =ship =path]
    ::  from the kernel
    ::  %behn
        [%wake ~]
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
