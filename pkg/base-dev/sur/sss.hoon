|%
++  lake
  |$  [rock wave]
  $_  ^?
  |%
  ++  name  *term
  +$  rock  ^rock
  +$  wave  ^wave
  ++  wash  |~  [rock wave]  *rock
  --
+$  aeon  @ud
+$  dude  dude:agent:gall
+$  what  ?(%rock %wave)
++  poke
  |%
  ++  request
    |*  paths=mold
    $:  path=paths
        =dude
        when=(unit aeon)
    ==
  ++  response
    |*  [=(lake) paths=mold]
    $:  path=paths
        =dude
        =aeon
        $%  [type=?(%nigh %yore %tomb) ~]
            $:  type=%scry
                $%  [what=%rock =rock:lake]
                    [what=%wave =wave:lake]
    ==  ==  ==  ==
  ++  on-rock
    |*  [=(lake) paths=mold]
    $:  path=paths
        src=ship
        from=dude
        stale=?
        fail=?
        =rock:lake
        wave=(unit wave:lake)
    ==
  --
--
