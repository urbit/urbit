::  Tests for ++have:guard:gall
::
/+  *test
::
|%
::
++  test-have-super
  %+  expect-eq
  !>  %.y
  !>  (have:guard:gall (sy [%super ~]~) [%write `%foo])
::
++  test-have-exact-and-empty
  ;:  weld
    %+  expect-eq
    !>  %.y
    !>  (have:guard:gall `(set perm:gall)`(sy [[%watch `%foo /] [%fling ~] ~]) [%fling ~])
  ::
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall (sy [[%watch `%foo /] [%fling ~] ~]) [%write ~])
  ::
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall ~ [%write ~])
  ==
::
++  test-have-write
  ;:  weld
    %+  expect-eq
    !>  %.y
    !>  (have:guard:gall (sy [[%write ~] ~]) [%write `%foo])
  ::
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall (sy [[%write `%foo] ~]) [%write `%bar])
  ::
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall (sy [[%write `%foo] ~]) [%write ~])
  ==
::
++  test-have-watch
  ;:  weld
    %+  expect-eq
    !>  %.y
    !>  (have:guard:gall (sy [[%watch ~ /foo] ~]) [%watch `%bar /foo/baz])
  ::
    %+  expect-eq
    !>  %.y
    !>  (have:guard:gall (sy [[%watch `%foo /foo] ~]) [%watch `%foo /foo/bar/bus])
  ::
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall (sy [[%watch `%foo /foo] ~]) [%watch `%bar /foo])
  ::
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall (sy [[%watch ~ /foo/bar] ~]) [%watch ~ /foo/baz])
  ::
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall (sy [[%watch ~ /foo/bar/baz] ~]) [%watch ~ /foo/bar])
  ==
::
++  test-have-reads  ::  dash
  ;:  weld
    %+  expect-eq
    !>  %.y
    !>  (have:guard:gall (sy [[%reads %c ~ ~ ~] ~]) [%reads %c `%x `%desk /foo/bar])
  ::
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall (sy [[%reads %c `%x ~ ~] ~]) [%reads %c `%y ~ ~])
  ::
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall (sy [[%reads %c ~ `%desk ~] ~]) [%reads %c ~ `%new-desk ~])
  ::
    %+  expect-eq
    !>  %.y
    !>  (have:guard:gall (sy [[%reads %c ~ ~ /foo] ~]) [%reads %c ~ ~ /foo/bar/txt])
  ==
::
++  test-have-cash
  ;:  weld
    %+  expect-eq
    !>  %.y
    !>  (have:guard:gall (sy [[%clay %local ~ ~ ~] ~]) [%clay %local `%x `%desk /foo])
  ::
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall (sy [[%clay %local `%x ~ ~] ~]) [%clay %local `%y ~ ~])
  ==
::
++  test-have-dash
  ;:  weld
    %+  expect-eq
    !>  %.y
    !>  (have:guard:gall (sy [[%ames %reads ~ /] ~]) [%ames %reads `%desk /foo/bar/txt])
  ::
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall (sy [[%ames %reads `%desk /foo/bus] ~]) [%ames %reads `%desk /foo/bar/txt])
  ::
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall (sy [[%ames %reads `%desk ~] ~]) [%ames %reads `%new-desk ~])
  ::
    %+  expect-eq
    !>  %.y
    !>  (have:guard:gall (sy [[%clay %write ~ /foo] ~]) [%clay %write `%desk /foo/bar])
  ==
::
++  test-have-unit-desk
  ;:  weld
    %+  expect-eq
    !>  %.y
    !>  (have:guard:gall (sy [[%clay %rules ~] ~]) [%clay %rules `%desk])
  ::
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall (sy [[%clay %rules `%bar] ~]) [%clay %rules ~])
  ==
::
++  test-have-unit-dude
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall (sy [[%gall %clear `%foo] ~]) [%gall %clear `%bar])
  ==
::
++  test-have-eyre-serve
  ;:  weld
    %+  expect-eq
    !>  %.y
    !>  (have:guard:gall (sy [[%eyre %serve /] ~]) [%eyre %serve /foo/bar])
  ::
    %+  expect-eq
    !>  %.y
    !>  (have:guard:gall (sy [[%eyre %serve /foo] ~]) [%eyre %serve /foo/bar/baz])
  ::
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall (sy [[%eyre %serve /foo/bar] ~]) [%eyre %serve /foo/baz])
  ::
    %+  expect-eq
    !>  %.n
    !>  (have:guard:gall (sy [[%eyre %serve /foo/bar/baz] ~]) [%eyre %serve /foo/bar])
  ==
::
++  do-must
  |=(=card:agent:gall (must:guard:gall ~dev card))
::
++  test-must
  ;:  weld
    ::  %give is always allowed
    %+  expect-eq
    !>  &
    !>  (do-must [%give %fact ~[/path] %noun !>(~)])
  ::
    ::  %slip is never allowed
    %+  expect-eq
    !>  |
    !>  (do-must [%slip [%arvo %ames %prod ~]])
  ::
    ::  %dole
    %+  expect-eq
    !>  [%super ~]~
    !>  (do-must [%pass /path %dole %foo [%agent [~dev %bar] %poke %noun !>(~)]])
  ::
    ::  %syscall
    %+  expect-eq
    !>  [%super ~]~
    !>  (do-must [%pass /path %arvo %syscall *note-arvo])
  ::
    ::  %watch
    %+  expect-eq
    !>  [%watch `%foo /foo/bar/bus]~
    !>  (do-must [%pass /path %agent [~dev %foo] %watch /foo/bar/bus])
  ::
    ::  %leave
    %+  expect-eq
    !>  &
    !>  (do-must [%pass /path %agent [~dev %foo] %leave ~])
  ::
    ::  %poke
    %+  expect-eq
    !>  [%write `%foo]~
    !>  (do-must [%pass /path %agent [~dev %foo] %poke %noun !>(~)])
  ==
::
++  test-must-ames
  =/  keen-path  /c/x/0/desk/foo/bar
  ;:  weld
    ::  %prod(%cong %stir)
    %+  expect-eq
    !>  [%super ~]~
    !>  (do-must [%pass /path %arvo %ames %prod ~[~bus]])
  ::
    ::  %sift(%spew)
    %+  expect-eq
    !>  [%ames %debug ~]~
    !>  (do-must [%pass /path %arvo %ames %sift ~[~zod]])
  ::
    ::  %snub
    %+  expect-eq
    !>  [%ames %block ~]~
    !>  (do-must [%pass /path %arvo %ames %snub %deny ~[~zod]])
  ::
    ::  %keen local ship, valid path
    %+  expect-eq
    !>  [%ames [%reads `%desk /foo/bar]]~
    !>  (do-must [%pass /path %arvo %ames %keen %.n [~dev keen-path]])
  ::
    ::  %keen remote ship
    %+  expect-eq
    !>  [%ames %keens ~]~
    !>  (do-must [%pass /path %arvo %ames %keen %.n [~zod keen-path]])
  ::
    ::  %keen local ship, not qualified path
    %+  expect-eq
    !>  |
    !>  (do-must [%pass /path %arvo %ames %keen %.n [~dev /foo]])
  ::
    ::  %yawn
    %+  expect-eq
    !>  &
    !>  (do-must [%pass /path %arvo %ames %yawn [~dev /foo/bar]])
  ::
    ::  %grow(%tomb %cull %tend %germ %snip)
    %+  expect-eq
    !>  [%ames %write ~]~
    !>  (do-must [%pass /path %arvo %ames %grow /foo %noun !>(~)])
  ::
    ::  %trim
    %+  expect-eq
    !>  [%super ~]~
    !>  (do-must [%pass /path %arvo %ames %trim 0])
  ==
::
++  test-must-behn
  ;:  weld
    %+  expect-eq
    !>  [%behn %timer ~]~
    !>  (do-must [%pass /path %arvo %behn %wait ~2000.1.1])
  ::
    %+  expect-eq
    !>  &
    !>  (do-must [%pass /path %arvo %behn %rest ~2000.1.1])
  ==
::
++  test-must-clay
  ;:  weld
    ::  %read local %sing
    %+  expect-eq
    !>  [%clay %local `%x `%base /foo]~
  (do-must [%pass /path %arvo %clay %read 0 ~dev %base [%sing %x [%ud 1] /foo]])
  ::
    ::  %read local %many
    %+  expect-eq
    !>  [%clay %local ~ `%base /foo]~
    !>  (do-must [%pass /path %arvo %clay %read 0 ~dev %base [%many & `moat:clay`[da+~2000.1.1 da+~2001.1.2 /foo]]])
  ::
    ::  %read local %mult empty paths expect always allowed
    %+  expect-eq
    !>  &
    !>  (do-must [%pass /path %arvo %clay %read 0 ~dev %base [%mult [%ud 1] ~]])
  ::
    ::  %read local %mult with paths
    %+  expect-eq
    !>  [[%clay %local `%x `%base /foo] [%clay %local `%u `%base /foo]~]
    !>  (do-must [%pass /path %arvo %clay %read 0 ~dev %base [%mult [%ud 1] (sy ~[[%x /foo] [%u /foo]])]])
  ::
    ::  %read foreign ship
    %+  expect-eq
    !>  [%clay %peers ~]~
    !>  (do-must [%pass /path %arvo %clay %read 0 ~zod %base [%sing %x [%ud 1] /foo]])
  ::
    ::  %rest is always allowed
    %+  expect-eq
    !>  &
    !>  (do-must [%pass /path %arvo %clay %rest 0])
  ::
    ::  %rite empty sob always allowed, no changes, no permissions
    %+  expect-eq
    !>  &
    !>  (do-must [%pass /path %arvo %clay %rite %base ~])
  ::
    ::  %rite with data
    %+  expect-eq
    !>  [%clay %write `%base /foo]~
    !>  (do-must [%pass /path %arvo %clay %rite %base [[/foo [%del ~]] ~]])
  ::
    ::  %name(%merg %drop %fuse)
    %+  expect-eq
    !>  [%clay %write `%base /]~
    !>  (do-must [%pass /path %arvo %clay %name %base %v1 ~])
  ::
    ::  %cred(%crow)
    %+  expect-eq
    !>  [%clay %crews ~]~
    !>  (do-must [%pass /path %arvo %clay %cred %admin ~])
  ::
    ::  %perm
    %+  expect-eq
    !>  [%clay %rules `%base]~
    !>  (do-must [%pass /path %arvo %clay %perm %base / [%r ~]])
  ::
    ::  %esse(%rein %zest)
    %+  expect-eq
    !>  [%clay %desks `%foo]~
    !>  (do-must [%pass /path %arvo %clay %esse %foo %.y])
  ::
    ::  %zeal empty lit always allowed, no changes, no perms
    %+  expect-eq
    !>  &
    !>  (do-must [%pass /path %arvo %clay %zeal ~])
  ::
    ::  %zeal
    %+  expect-eq
    !>  [[%clay %desks `%foo] [%clay %desks `%bar] ~]
    !>  (do-must [%pass /path %arvo %clay %zeal [[%foo %live] [%bar %live] ~]])
  ::
    ::  %tire
    %+  expect-eq
    !>  [%clay %pulse ~]~
    !>  (do-must [%pass /path %arvo %clay %tire ~])
  ::
    ::  %seal
    %+  expect-eq
    !>  [%clay %perms ~]~
    !>  (do-must [%pass /path %arvo %clay %seal %foo %.y (sy [%super ~]~)])
  ::
    ::  %ward
    %+  expect-eq
    !>  [%clay %guard ~]~
    !>  (do-must [%pass /path %arvo %clay %ward ~])
  ::
    ::  %mont(%ogre %dirk)
    %+  expect-eq
    !>  [%clay %mount ~]~
    !>  (do-must [%pass /path %arvo %clay %mont %pot [[~dev %foo [%ud 1]] /]])
  ::
    ::  %tomb %pick expect always allowed
    %+  expect-eq
    !>  &
    !>  (do-must [%pass /path %arvo %clay %tomb [%pick ~]])
  ::
    ::  %tomb %norm(%worn %seek)
    %+  expect-eq
    !>  [%clay %stone `%foo]~
    !>  (do-must [%pass /path %arvo %clay %tomb `clue:clay`[%norm ~dev %foo *norm:clay]])
  ::
    ::  %tomb default case (e.g. %all) expect wildcard stone
    %+  expect-eq
    !>  [%clay %stone ~]~
    !>  (do-must [%pass /path %arvo %clay %tomb [%all ~]])
  ==
::
++  test-must-dill
  ;:  weld
    ::  %logs
    %+  expect-eq
    !>  [%dill %sylog ~]~
    !>  (do-must [%pass /path %arvo %dill %logs ~])
  ::
    ::  %crud(%talk %text)
    %+  expect-eq
    !>  [%dill %print ~]~
    !>  (do-must [%pass /path %arvo %dill %crud %err ~])
  ::
    ::  %mass
    %+  expect-eq
    !>  [%dill %weigh ~]~
    !>  (do-must [%pass /path %arvo %dill %mass ~])
  ::
    ::  %meld(%pack)
    %+  expect-eq
    !>  [%dill %press ~]~
    !>  (do-must [%pass /path %arvo %dill %meld ~])
  ==
::
++  test-must-eyre
  ;:  weld
    ::  %eauth-host(%rule)
    %+  expect-eq
    !>  [%eyre %setup ~]~
    !>  (do-must [%pass /path %arvo %eyre %eauth-host ~])
  ::
    ::  %connect: serve path is path.binding
    %+  expect-eq
    !>  [%eyre %serve /foo/bar]~
    !>  (do-must [%pass /path %arvo %eyre %connect [~ /foo/bar] %gen])
  ::
    ::  %set-response: parsed url path
    %+  expect-eq
    !>  [%eyre %serve /foo/bar]~
    !>  (do-must [%pass /path %arvo %eyre %set-response '/foo/bar' ~])
  ::
    ::  %set-response: unparseable url to path
    %+  expect-eq
    !>  |
    !>  (do-must [%pass /path %arvo %eyre %set-response '' ~])
  ::
    ::  %disconnect is always allowed
    %+  expect-eq
    !>  &
    !>  (do-must [%pass /path %arvo %eyre %disconnect [~ /foo]])
  ::
    ::  %approve-origin(%reject-origin)
    %+  expect-eq
    !>  [%eyre %cross ~]~
    !>  (do-must [%pass /path %arvo %eyre %approve-origin 'example.com'])
  ::
    ::  %spew
    %+  expect-eq
    !>  [%eyre %debug ~]~
    !>  (do-must [%pass /path %arvo %eyre %spew 0])
  ==
::
++  test-must-gall
  ;:  weld
    ::  %nuke
    %+  expect-eq
    !>  [%gall %clear `%foo]~
    !>  (do-must [%pass /path %arvo %gall %nuke %foo])
  ::
    ::  %spew(%sift)
    %+  expect-eq
    !>  [%gall %debug ~]~
    !>  (do-must [%pass /path %arvo %gall %spew ~])
  ::
    ::  %sear(%trim)
    %+  expect-eq
    !>  [%super ~]~
    !>  (do-must [%pass /path %arvo %gall %sear ~zod])
  ==
::
++  test-must-iris
  ;:  weld
    %+  expect-eq
    !>  [%iris %fetch ~]~
    !>  (do-must [%pass /path %arvo %iris %request *request:http *outbound-config:iris])
  ::
    %+  expect-eq
    !>  &
    !>  (do-must [%pass /path %arvo %iris %cancel-request ~])
  ==
::
++  test-must-jael
  ;:  weld
    %+  expect-eq
    !>  [%jael %privy ~]~
    !>  (do-must [%pass /path %arvo %jael %private-keys ~])
  ::
    ::  %public-keys(%turf) always allowed
    %+  expect-eq
    !>  &
    !>  (do-must [%pass /path %arvo %jael %public-keys ~])
  ::
    %+  expect-eq
    !>  [%jael %watch ~]~
    !>  (do-must [%pass /path %arvo %jael %listen ~ *source:jael])
  ::
    %+  expect-eq
    !>  [%jael %moons ~]~
    !>  (do-must [%pass /path %arvo %jael %moon ~dev *udiff:point:jael])
  ::
    %+  expect-eq
    !>  [%jael %rekey ~]~
    !>  (do-must [%pass /path %arvo %jael %rekey 1 0])
  ::
    %+  expect-eq
    !>  [%jael %login ~]~
    !>  (do-must [%pass /path %arvo %jael %step ~])
  ::
    %+  expect-eq
    !>  [%jael %blast ~]~
    !>  (do-must [%pass /path %arvo %jael %ruin ~])
  ==
::
++  test-must-khan-lick
  ;:  weld
    ::  %fard(%lard)
    %+  expect-eq
    !>  [%khan %twine ~]~
    !>  (do-must [%pass /path %arvo %khan %fard %base [%base %thread %noun !>(~)]])
  ::
    ::  %spin(%shut %spit)
    %+  expect-eq
    !>  [%lick %ports ~]~
    !>  (do-must [%pass /path %arvo %lick %spin /my-port])
  ==
--
