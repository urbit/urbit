/-  *docket
|%
::
++  mime
  |%
  +$  draft
    $:  title=(unit @t)
        info=(unit @t)
        color=(unit @ux)
        glob=(unit url)
        base=(unit term)
        version=(unit version)
        website=(unit url)
        license=(unit cord)
    ==
  ::
  ++  finalize
    |=  =draft
    ^-  (unit docket)
    ?~  title.draft  ~
    ?~  info.draft  ~
    ?~  color.draft  ~
    ?~  glob.draft  ~
    ?~  base.draft  ~
    ?~  version.draft  ~
    ?~  website.draft  ~
    ?~  license.draft  ~
    =,  draft
    :-  ~
    :*  %1
        u.title
        u.info
        u.color
        u.glob
        u.base
        u.version
        u.website
        u.license
    ==
  ::
  ++  from-clauses
    =|  =draft
    |=  cls=(list clause)
    ^-  (unit docket)
    =*  loop  $
    ?~  cls  (finalize draft)
    =*  clause  i.cls
    =.  draft
      ?-  -.clause
        %title  draft(title `title.clause)
        %info   draft(info `info.clause)
        %color  draft(color `color.clause)
        %glob   draft(glob `url.clause)
        %base   draft(base `base.clause)
        %version  draft(version `version.clause)
        %website  draft(website `website.clause)
        %license  draft(license `license.clause)
      ==
    loop(cls t.cls)
  ::
  ++  to-clauses
    |=  d=docket
    ^-  (list clause)
    :~  title+title.d
        info+info.d
        color+color.d
        glob+glob.d
        base+base.d
        version+version.d
        website+website.d
        license+license.d
    ==
  ::
  ++  spit-clause
    |=  =clause
    ^-  tape
    %+  weld  "  {(trip -.clause)}+"
    ?-    -.clause
      ?(%title %info %glob %website %license %base)  "'{(trip +.clause)}'"
      %color                                   (scow %ux color.clause)
      ::
        %version
      =,  version.clause
      "[{(scow %ud major)} {(scow %ud minor)} {(scow %ud patch)}]"
    ==
  ::
  ++  spit-docket
    |=  dock=docket
    ^-  tape
    ;:  welp
      ":~\0a"
      `tape`(zing (join "\0a" (turn (to-clauses dock) spit-clause)))
      "\0a=="
    ==
  --
::
++  enjs
  =,  enjs:format
  |%
  ::
  ++  update
    |=  u=^update
    ~&  u
    ^-  json
    %+  frond  -.u
    ^-  json
    ?-  -.u
      %del-dock  s+desk.u
    ::
        %initial  
      %-  pairs
      %+  turn  ~(tap by initial.u)
      |=([=desk d=^docket] [desk (docket d)]) 
    ::
        %add-dock
      %-  pairs
      :~  desk+s+desk.u
          docket+(docket docket.u)
      ==
    ==
  ::
  ++  num
    |=  a=@u
    ^-  ^tape
    =/  p=json  (numb a)
    ?>  ?=(%n -.p)
    (trip p.p)
  ::

  ++  version
    |=  v=^version
    ^-  json
    :-  %s
    %-  crip
    "{(num major.v)}.{(num minor.v)}.{(num patch.v)}"

  ++  merge
    |=  [a=json b=json]
    ^-  json
    ?>  &(?=(%o -.a) ?=(%o -.b))
    [%o (~(uni by p.a) p.b)]
  ::
  ++  docket
    |=  d=^docket
    ^-  json
    %-  pairs
    :~  title+s+title.d
        info+s+info.d
        color+s+(scot %ux color.d)
        glob+s+glob.d
        base+s+base.d
        version+(version version.d)
        license+s+license.d
        website+s+website.d
    ==
  ::
  ++  case
    |=  c=^case
    %+  frond  -.c
    ?-  -.c
      %da   s+(scot %da p.c)
      %tas  s+(scot %tas p.c)  
      %ud   (numb p.c)
    ==
  ::
  ++  treaty
    |=  t=^treaty
    %+  merge  (docket docket.t)
    %-  pairs
    :~  ship+s+(scot %p ship.t)
        desk+s+desk.t
        cass+(case case.t)
        hash+s+(scot %uv hash.t)
    ==
  --
--
