!:
::  application standard library
::
=>
  |%                                                      ::  models
  ++  cron  ?(%day %hour %minute %none %second)           ::  wake frequency
  ++  lamp                                                ::  simple web app
            $_  ^?  |%                                    ::  opaque object
            ++  give                                      ::  serve
              |=  [now=@da fig=weev]                      ::  time, request
              :-  p=*(list gift)                          ::  act
              q=*(unit ,[p=(list slip) q=lamp])           ::  request/continue
            ::                                            ::
            ++  miss                                      ::  redirect?
              |=  [pac=pact ced=cred]                     ::  test and apply
              ^-  (unit purl)                             ::
              ~                                           ::
            --                                            ::
  ++  lump  ,[p=path q=mime r=octs]                       ::  submitted data
  ++  user  ?(%born %came %died %left)                    ::  user event
  ++  wick  vase                                          ::  vase of lamp
  ++  weev                                                ::  web event
            $%  [%cron p=cron]                            ::  wakeup
                [%form p=pact q=quay]                     ::  posted form
                [%note p=path q=note]                     ::  extrinsic note
                [%post p=pact q=mime q=octs]              ::  non-form post
                [%putt p=pact q=(list lump)]              ::  put
                [%user p=user]                            ::  user event
            ==                                            ::
  --                                                      ::
|%                                                        ::  functions
++  lunt                                                  ::  web framework
  |=  :*  who=ship                                        ::  owner
          ::  msg=tape                                        ::  prompt
          ped=cron                                        ::  wake frequency
          rut=(list rout)                                 ::  routes to
          ras=wick                                        ::  server state
      ==
  ^-  bowl
  =+  ^=  hup  ^-  (list slip)
      :~  [/ [%ht rut]]
          ::  [/ [%up %none msg]]
      ==
  :-  *(list gift)
  :-  ~
  :-  hup
  |=  [now=@da pax=path nut=note]
  ^-  bowl
  =+  [saw=*(list gift) ask=*(list slip)]
  =<  zing:wist
  |%
  ++  send                                                ::  dispatch event
    |=  bax=weev
    ^+  +>
    =+  sam=!>([now bax])
    =+  gat=(slap ras [%cnbc %give])
    =+  pro=(slam gat sam)
    =+  [wax=(slot 2 pro) hin=(slot 3 pro)]
    =.  saw  (weld ((hard (list gift)) q.wax) saw)
    ?:  =(~ q.hin)
      +>.$
    =+  [vis=(slot 6 hin) lym=(slot 7 hin)]
    =+  ^=  gin  ^-  (list slip)
        %+  turn  ((hard (list slip)) q.vis)
        |=(a=slip [[%lunt p.a] q.a])
    %=  +>.$
      ask    (weld ((hard (list slip)) q.vis) ask)
      q.ras  q.lym
    ==
  ::
  ++  pass                                                ::  try redirect
    |=  [pac=pact ced=cred]
    ^-  (unit purl)
    =+  sam=!>([pac ced])
    =+  gat=(slap ras [%cnbc %miss])
    =+  pro=(slam gat sam)
    ?:  =(~ q.pro)  ~
    =+  vur=(slot 3 pro)
    [~ ((hard purl) q.vur)]
  ::
  ++  post                                                ::  handle post
    |=  [rid=@ud zab=scab ced=cred mot=moth]
    ^+  +>
    ?>  ?=(^ r.mot)
    =+  cot=(need (~(get by q.mot) %content-type))
    =+  ^=  guz  ^-  (unit quay)
        ?.  =(cot ~['application/x-www-form-urlencoded'])  ~
        =+  vex=((full yquy:epur) [1 1] (trip q.u.r.mot))
        ?~  q.vex  ~
        [~ p.u.q.vex]
    %-  send
    ?~  guz
      [%post p.p.zab /application/octet-stream u.r.mot]   ::  XX parse cot
    [%form p.p.zab u.guz]
  ::
  ++  went                                                ::  handle get
    |=  [rid=@ud zab=scab ced=cred mot=moth]
    ^+  +>
    =+  sek=(roil [who now (shax (mix (sham zab) now)) ced] zab ras)
    =+  ^=  rep
        :+  %th  rid
        ?~  sek
          [%raw [404 ~ [~ (tact "http error 404 at {<now>}")]]]
        u.sek
    +>.$(saw [rep saw])
  ::
  ++  wist                                                ::  handle note
    ^+  .
    ?.  ?=(%ht -.nut)
      ?>  ?=([%lunt *] pax)
      (send [%note t.pax nut])
    =>  ?:(=(%post p.s.nut) (post +.nut) .)
    ?>  ?=(%ht -.nut)
    (went +.nut)
  ::
  ++  zing                                                ::  resolve
    ^-  bowl
    [saw [~ (weld ask hup) ..$]]
  --
::
++  roil
  |=  [mad=scad zab=scab ras=wick]
  ^-  (unit love)
  =+  cag=`path`(flop p.p.zab)
  ?>  ?=(^ cag)
  =+  syd=i.cag
  =+  lok=~(rent co ~ %da q.mad)
  =+  hox=~(rent co ~ %p p.mad)
  =+  tem=`path`[hox syd lok %web t.cag]
  =<  veen
  |%
  ++  drem
    |=  axt=@ta
    ^-  (unit love)
    =+  arc=((hard arch) .^(%cy tem))
    ?:  (~(has by r.arc) axt)
      =+  dat=((hard ,@) .^(%cx (weld tem `path`[axt ~])))
      :-  ~
      :+  %mid
        ?+  axt  [%application %octet-stream ~]
          %html  [%text %html ~]
          %txt   [%text %plain ~]
          %css   [%text %css ~]
          %js    [%text %javascript ~]
        ==
      [(met 3 dat) dat]
    ?.  (~(has by r.arc) %hoon)  ~
    :-  ~
    =+  vez=(vang & [hox syd lok t.cag])
    =+  dat=((hard ,@) .^(%cx (weld tem `path`[%hoon ~])))
    =+  gen=(scan (trip dat) (full (ifix [gay gay] tall:vez)))
    =+  pro=(slam (slam (slap ras gen) !>(mad)) !>(zab))
    ((hard love) q.pro)
  ::
  ++  dunt
    ^-  (unit love)
    (drem %html)
  ::
  ++  veen
    ?~(r.q.p.zab dunt (drem u.r.q.p.zab))
  --
--
