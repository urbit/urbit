/-  sur=hark-store
^?
=,  sur
=<  [. sur]
|%

++  enjs
  =,  enjs:format
  |%
  ++  update
    |=  upd=^update
    ^-  json
    |^
    %+  frond  -.upd
    ?+  -.upd  a+~
        %added      (notification +.upd)
        %add-note   (add-note +.upd)
        %timebox  (timebox +.upd)
        %more     (more +.upd)
        %read-each     (read-each +.upd)
        %read-count  (place +.upd)
        %unread-each  (read-each +.upd)
        %unread-count  (unread-count +.upd)
        %saw-place    (saw-place +.upd)
        %all-stats     (all-stats +.upd)
        %del-place     (place +.upd)
        ::%read-note   (index +.upd)
        ::%note-read   (note-read +.upd)
        %archived       (archived +.upd)
    ==
    ::
    ++  add-note
      |=  [bi=^bin bo=^body]
      %-  pairs
      :~  bin+(bin bi)
          body+(body bo)
      ==
    ::
    ++  saw-place
      |=  [p=^place t=(unit ^time)]
      %-  pairs
      :~  place+(place p)
          time+?~(t ~ (time u.t))
      ==
    ::
    ++  archived
      |=  [t=^time l=^lid n=^notification]
      %-  pairs
      :~  lid+(lid l)
          time+s+(scot %ud t)
          notification+(notification n)
      ==
    ::
    ++  note-read
      |=  *
      (pairs ~)
    ::
    ++  all-stats
      |=  places=(map ^place ^stats)
      ^-  json 
      :-  %a
      ^-  (list json)
      %+  turn  ~(tap by places)
      |=  [p=^place s=^stats]
      %-  pairs
      :~  stats+(stats s)
          place+(place p)
          
      ==
    ::
    ++  stats
      |=  s=^stats
      ^-  json
      %-  pairs
      :~  each+a+(turn ~(tap in each.s) (cork spat (lead %s)))
          last+(time last.s)
          count+(numb count.s)
      ==
    ++  more
      |=  upds=(list ^update)
      ^-  json
      a+(turn upds update)
    ::
    ++  place
      |=  =^place
      %-  pairs
      :~  desk+s+desk.place
          path+s+(spat path.place)
      ==
    ::
    ++  bin
      |=  =^bin
      %-  pairs
      :~  place+(place place.bin)
          path+s+(spat path.bin)
      ==
    ++  notification
      |=  ^notification
      ^-  json
      %-  pairs
      :~  time+(time date)
          bin+(^bin bin)
          body+(bodies body)
      ==
    ++  bodies
      |=  bs=(list ^body)
      ^-  json
      a+(turn bs body)
    ::
    ++  contents
      |=  cs=(list ^content)
      ^-  json
      a+(turn cs content)
    ::
    ++  content
      |=  c=^content
      ^-  json
      %+  frond  -.c
      ?-  -.c
        %ship  s+(scot %p ship.c)
        %text  s+cord.c
      ==
    ::
    ++  body
      |=  ^body
      ^-  json
      %-  pairs
      :~  title+(contents title)
          content+(contents content)
          time+(^time time)
          link+s+(spat link)
      ==
    :: 
    ++  binned-notification
      |=  [=^bin =^notification]
      %-  pairs
      :~  bin+(^bin bin)
          notification+(^notification notification)
      ==
    ++  lid
      |=  l=^lid
      ^-  json
      %+  frond  -.l
      ?-  -.l
        ?(%seen %unseen)  ~
        %archive             s+(scot %ud time.l)
      ==
    ::
    ++  timebox
      |=  [li=^lid l=(list ^notification)]
      ^-  json
      %-  pairs
      :~  lid+(lid li)
          notifications+a+(turn l notification)
      ==
    ::
    ++  read-each
      |=  [p=^place pax=^path]
      %-  pairs
      :~  place+(place p)
          path+(path pax)
      ==
    ::
    ++  unread-count
      |=  [p=^place inc=? count=@ud]
      %-  pairs
      :~  place+(place p)
          inc+b+inc
          count+(numb count)
      ==
    --
  --
++  dejs
  =,  dejs:format
  |%
  :: TODO: fix +stab 
  ::
  ++  pa
    |=  j=json
    ^-  path
    ?>  ?=(%s -.j)
    ?:  =('/' p.j)  /
    (stab p.j)
  ::
  ++  place
    %-  ot
    :~  desk+so
        path+pa
    ==
  ::
  ++  bin
    %-  ot
    :~  path+pa
        place+place
    ==
  ::
  ++  read-each
    %-  ot
    :~  place+place
        path+pa
    ==
  ::
  ::  parse date as @ud
  ::    TODO: move to zuse
  ++  sd
    |=  jon=json 
    ^-  @da
    ?>  ?=(%s -.jon)
    `@da`(rash p.jon dem:ag)
  ::
  ++  lid
    %-  of
    :~  archive+sd
        unseen+ul
        seen+ul
    ==
  ::
  ++  archive
    %-  ot
    :~  lid+lid
        bin+bin
    ==
  ::
  ++  action
    ^-  $-(json ^action)
    %-  of
    :~  archive-all+ul
        archive+archive
        opened+ul
        read-count+place
        read-each+read-each
        read-note+bin
    ==
  --
--
