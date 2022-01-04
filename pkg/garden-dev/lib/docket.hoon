/-  *docket
|%
::
++  mime
  |%
  +$  draft
    $:  title=(unit @t)
        info=(unit @t)
        color=(unit @ux)
        glob-http=(unit [=url hash=@uvH])
        glob-ames=(unit [=ship hash=@uvH])
        base=(unit term)
        site=(unit path)
        image=(unit url)
        version=(unit version)
        website=(unit url)
        license=(unit cord)
    ==
  ::
  ++  finalize-0
    |=  =draft
    ^-  (unit docket-0)
    ?~  title.draft  ~
    ?~  info.draft  ~
    ?~  color.draft  ~
    ?~  version.draft  ~
    ?~  website.draft  ~
    ?~  license.draft  ~
    =/  href-0=(unit href-0)
      ?^  site.draft  `[%site u.site.draft]
      ?~  base.draft  ~
      ?^  glob-http.draft
        `[%glob u.base hash.u.glob-http %http url.u.glob-http]:draft
      ?~  glob-ames.draft
        ~
      `[%glob u.base hash.u.glob-ames %ames ship.u.glob-ames]:draft
    ?~  href-0  ~
    =,  draft
    :-  ~
    :*  %1
        u.title
        u.info
        u.color
        u.href-0
        image
        u.version
        u.website
        u.license
    ==
  ::
  ++  finalize
    |=  =draft
    ^-  (unit docket)
    ?~  title.draft  ~
    ?~  info.draft  ~
    ?~  color.draft  ~
    ?~  version.draft  ~
    ?~  website.draft  ~
    ?~  license.draft  ~
    =/  =spot
      ?~  base.draft  ~
      ?^  glob-http.draft
        `[u.base hash.u.glob-http %http url.u.glob-http]:draft
      ?~  glob-ames.draft
        ~
      `[u.base hash.u.glob-ames %ames ship.u.glob-ames]:draft
    =/  href=(unit href)
      ?^  site.draft  `[spot %site u.site.draft]
      `[spot %glob ~]
    ?~  href  ~
    =,  draft
    :-  ~
    :*  %1
        u.title
        u.info
        u.color
        u.href
        image
        u.version
        u.website
        u.license
    ==
  ::
  ++  from-clauses-0
    =|  =draft
    |=  cls=(list clause)
    ^-  (unit docket-0)
    =*  loop  $
    ?~  cls  (finalize-0 draft)
    =*  clause  i.cls
    =.  draft
      ?-  -.clause
        %title  draft(title `title.clause)
        %info   draft(info `info.clause)
        %color  draft(color `color.clause)
        %glob-http   draft(glob-http `[url hash]:clause)
        %glob-ames   draft(glob-ames `[ship hash]:clause)
        %base   draft(base `base.clause)
        %site   draft(site `path.clause)
        %image  draft(image `url.clause)
        %version  draft(version `version.clause)
        %website  draft(website `website.clause)
        %license  draft(license `license.clause)
      ==
    loop(cls t.cls)
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
        %glob-http   draft(glob-http `[url hash]:clause)
        %glob-ames   draft(glob-ames `[ship hash]:clause)
        %base   draft(base `base.clause)
        %site   draft(site `path.clause)
        %image  draft(image `url.clause)
        %version  draft(version `version.clause)
        %website  draft(website `website.clause)
        %license  draft(license `license.clause)
      ==
    loop(cls t.cls)
  ::
  ++  to-clauses
    |=  d=docket
    ^-  (list clause)
    %-  zing
    :~  :~  title+title.d
            info+info.d
            color+color.d
            version+version.d
            website+website.d
            license+license.d
        ==
        ?~  image.d  ~  ~[image+u.image.d]
        ?.  ?=(%site -.+.href.d)  ~  ~[site+path.href.d]
        ?:  ?=(~ spot.href.d)  ~
          =/  ref  glob-reference.u.spot.href.d
          :~
            base+base.u.spot.href.d
            ?-  -.location.ref
              %http  [%glob-http url.location hash]:ref
              %ames  [%glob-ames ship.location hash]:ref
    ==  ==  ==
  ::
  ++  to-clauses-0
    |=  d=docket-0
    ^-  (list clause)
    %-  zing
    :~  :~  title+title.d
            info+info.d
            color+color.d
            version+version.d
            website+website.d
            license+license.d
        ==
        ?~  image.d  ~  ~[image+u.image.d]
        ?:  ?=(%site -.href-0.d)  ~[site+path.href-0.d]
        =/  ref=glob-reference  glob-reference.href-0.d
        :~  base+base.href-0.d
            ?-  -.location.ref
              %http  [%glob-http url.location.ref hash.ref]
              %ames  [%glob-ames ship.location.ref hash.ref]
    ==  ==  ==
  ::
  ++  spit-clause
    |=  =clause
    ^-  tape
    %+  weld  "  {(trip -.clause)}+"
    ?+  -.clause  "'{(trip +.clause)}'"
      %color  (scow %ux color.clause)
      %site   (spud path.clause)
    ::
        %glob-http
      "['{(trip url.clause)}' {(scow %uv hash.clause)}]"
    ::
        %glob-ames
      "[{(scow %p ship.clause)} {(scow %uv hash.clause)}]"
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
  ::
  ++  spit-docket-0
    |=  dock=docket-0
    ^-  tape
    ;:  welp
      ":~\0a"
      `tape`(zing (join "\0a" (turn (to-clauses-0 dock) spit-clause)))
      "\0a=="
    ==
  --
::
++  enjs
  =,  enjs:format
  |%
  ::
  ++  charge-update
    |=  u=^charge-update
    ^-  json
    %+  frond  -.u
    ^-  json
    ?-  -.u
      %del-charge  s+desk.u
    ::
        %initial
      %-  pairs
      %+  turn  ~(tap by initial.u)
      |=([=desk c=^charge] [desk (charge c)])
    ::
        %add-charge
      %-  pairs
      :~  desk+s+desk.u
          charge+(charge charge.u)
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
  ::
  ++  merge
    |=  [a=json b=json]
    ^-  json
    ?>  &(?=(%o -.a) ?=(%o -.b))
    [%o (~(uni by p.a) p.b)]
  ::
  ++  spot
    |=  s=^spot
    ^-  json
    ?~  s  ~
    %-  pairs
      :~  base+s+base.u.s
          glob-reference+(glob-reference glob-reference.u.s)
      ==
  ::
  ++  href-0
    |=  h=^href-0
    %+  frond  -.h
    ?-    -.h
        %site  s+(spat path.h)
        %glob
      %-  pairs
      :~  base+s+base.h
          glob-reference+(glob-reference glob-reference.h)
        ==
    ==
  ::
  ++  href
    |=  h=^href
    %+  frond  +<.h
    ?-    +<.h
        %site  s+(spat path.h)
        %glob  (spot spot.h)
    ==
  ::
  ++  glob-reference
    |=  ref=^glob-reference
    %-  pairs
    :~  hash+s+(scot %uv hash.ref)
        location+(glob-location location.ref)
    ==
  ::
  ++  glob-location
    |=  loc=^glob-location
    ^-  json
    %+  frond  -.loc
    ?-  -.loc
      %http  s+url.loc
      %ames  s+(scot %p ship.loc)
    ==
  ::
  ++  charge
    |=  c=^charge
    %+  merge  (docket docket.c)
    %-  pairs
    :~  chad+(chad chad.c)
    ==
  ::
  ++  docket
    |=  d=^docket
    ^-  json
    %-  pairs
    :~  title+s+title.d
        info+s+info.d
        color+s+(scot %ux color.d)
        href+(href href.d)
        image+?~(image.d ~ s+u.image.d)
        version+(version version.d)
        license+s+license.d
        website+s+website.d
    ==
  ++  docket-0
    |=  d=^docket-0
    ^-  json
    %-  pairs
    :~  title+s+title.d
        info+s+info.d
        color+s+(scot %ux color.d)
        href-0+(href-0 href-0.d)
        image+?~(image.d ~ s+u.image.d)
        version+(version version.d)
        license+s+license.d
        website+s+website.d
    ==
  ::
  ++  chad
    |=  c=^chad
    %+  frond  -.c
    ?+  -.c  ~
      %hung  s+err.c
    ==
  --
--
