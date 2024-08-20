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
++  poke
  |%
  ++  request
    |*  paths=mold
    $:  path=paths
        =dude
    ==
  ++  response
    |*  [=(lake) paths=mold]
    $:  path=paths
        =dude
        $%  [what=%tomb ~]
            [what=%rock =aeon =rock:lake]
            [what=%wave =aeon =wave:lake]
    ==  ==
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
