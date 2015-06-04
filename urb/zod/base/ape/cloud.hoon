::  digital ocean fleet management
::
::::
  ::
/?  314
/-  *talk
/+  talk, sole, http

::
::
::::  sivtyv-barnel
  ::
             
!:
|%
++  instance
$:  plat=?(%do %gce)  name=@t  id=@t  status=@t  created=@da  snapshot=name=@t  ::disk=@u  region=@t  
ip=(list ,@if)
==
++  create-req-do
$:
name=@t  ::region=@t
size=@t  image=@t  ssh=(list cord)
backups=(unit ,?)  ipv6=(unit ,?)
private-networking=(unit ,?)  user-data=(unit ,@t)
==
++  create-req-gce  ,[project=@t zone=@t name=@t machine-type=@t]
++  axle
$:  auth=[do=keys gce=keys]  toke=[do=tokens gce=tokens]
    insts=(map ,@t instance)
==
++  keys  ,[authc=(unit ,@t) client-secret=(unit ,@t)]
++  tokens  ,[access=@t refresh=@t]
++  move  ,[bone card]
++  card
$%  [%diff %json json]
    [%wait wire @da]
    [%send wire [ship term] %poke %talk-command command]
    [%them wire (unit hiss)]
==
--
!:
|% :::
++  parse-iso8601
  =<  (cook to-time (parsf ;"{parse-day}T{parse-seconds}{parse-zone}"))
  |%
  ++  to-time
     |=  [[y=@u m=@u d=@u] t=[h=@u m=@u s=@u ms=@u] [syn=? zh=@u zm=@u]]
     ^-  @da
     %-  year
     ^-  date
     =:  h.t  ?:(syn (sub h.t zh) (add h.t zh))
         m.t  ?:(syn (sub m.t zm) (add m.t zm))
       == 
     [[& y] m d h.t m.t s.t (div (mul ms.t 0x1.0000) 1.000) ~]
  ++  parse-day    (parsf ;"{dem}\-{dem}\-{dem}")
  ++  parse-seconds  (parsf ;"{dem}:{dem}:{dem}{(optional ;~(pfix dot dem))}")
  ++  optional  |*(fel=_rule ;~(pose fel (easy 0)))
  ++  parse-zone
    ;~  pose 
      (cold [& 0 0] (jest 'Z')) 
      (parsf ;"{parse-zone-sign}{dem}:{dem}")
    ==
  ++  parse-zone-sign  ;~(plug ;~(pose (cold & lus) (cold | hep)))
  --
++  key-do
  (mo [%start 'power_on'] [%stop 'shutdown'] [%reboot 'power_cycle'] ~)
++  adapter-do
  |=  a=cord
  (~(got by key-do) a)
++  parse-ip-do
  =>  jo
  %-  ot  
  :_  ~  v4/(ar (ot 'ip_address'^(su lip:ag) ~))
++  parse-ip-gce
  =>  jo
  %+  cu  |=(a=(list (list ,@if)) `(list ,@if)`(zing a))
  (ar (ot 'accessConfigs'^(ar (ot 'natIP'^(su lip:ag) ~)) ~))
++  tail-url
  |=  a=cord
  -:(flop q.q:(need (epur a)))
++  parse-region
  =>  jo
  (ot name/so ~)
++  parse-id-text
  |=  jon=json
  ?.(?=([?(%n %s) *] jon) ~ (some p.jon))
++  create-do-body
  |=  $:  name=@t  ::region=@t
      size=@t  image=@t  ssh-keys=(list cord)
      backups=(unit ,?)  ipv6=(unit ,?)  private-networking=(unit ,?)  user-data=(unit ,@t)  
  ==
  %-  jobe
  :~  name/s/name  ::region/s/region
      size/s/size  image/s/image ::(jone image)
       backups/?~(backups ~ b/u.backups)  ipv6/?~(ipv6 ~ b/u.ipv6)
      'user_data'^?~(user-data ~ s/u.user-data)  'private_networking'^?~(private-networking ~ b/u.private-networking)
  ==
++  convert-do
  |=  a=?(%start %stop %reboot)
  ?-  a
    %start
  'power_on'
    %stop
  'shutdown'
    %reboot
  'power_cycle'
  ==
++  state-to-json
  |=  a=(list instance)
  :-  %a
  %+  turn  a
  |=  instance
  ^-  json
  %-  jobe
  :~  name/`json`s/name
      id/s/id
      status/s/status
      created/s/(crip (dust (yore created)))
      ::region/s/region
      snapshot/s/snapshot
  ::    disk/`json`(jone disk)
      ip/a/(turn ip |=(a=@if s/(rsh 3 1 (scot %if a))))
  ==
--
!:
|_  [bowl vat=axle]
::
++  prep  ,_`.
::
++  peer
  |=  pax=path
  ^-  [(list move) _+>.$]
  :_  +>.$
  =+  lis=(~(tap by insts.vat))
  [ost %diff %json (state-to-json (turn lis |=(a=[@t instance] +.a)))]~
::
++  spam
  |=  jon=json
  %+  turn  (~(tap by sup))
  |=  [sub=bone @ pax=path]
  ^-  move
  [sub %diff %json jon]
++  httpreq
    |=  $:  pour-path=wire
            domain=(list cord)  end-point=path
          req-type=$?(%get %delt [%post json])  headers=math
        queries=quay
        ==
    ^-  move
    =+  ^-  parsed-url=purl
        :+  ^=  host-port                               ::  ++hart
            :+  security=%.y
              port=~
            host=[%.y [path=[%com domain]]]
          endpoint=[extensions=~ point=end-point]       ::  ++pork,
        q-strings=queries                               ::  ++quay
    =+  ^-  request=hiss                                ::  cast to hiss
        :-  parsed-url
        ?@  req-type
          [req-type headers ~]
        [%post headers ~ (tact (pojo +.req-type))]
    :^  ost  %them  pour-path
    `(unit hiss)`[~ request]
::
++  auth-queries  
    |=  code=cord
    :~  'grant_type'^'authorization_code'
        'code'^code
        :-  'client_id'
        'd8f46b95af38c1ab3d78ad34c2157a6959c23eb0eb5d8e393f650f08e6a75c6f'
        'redirect_uri'^'http://localhost:8443/home/pub/cloud/fab'
    ==
::
++  poke-cloud-auth
  |=  [cde=cord typ=cord]
  ^-  [(list move) _+>.$]
  ?:  =(%do typ) 
      =.  authc.do.auth.vat
      [~ cde]
    :_  +>.$  ~
  =.  access.gce.toke.vat
  cde
  :_  +>.$
  :-  list-instances-gce 
  ~[(publish [%lin & 'successfully authenticated to gce']~)]
::
++  poke-cloud-secret
  |=  [secret=cord typ=cord]
  ^-  [(list move) _+>.$]
  ?+    typ  ~|(missing-platform=typ !!)
      %do
    =.  client-secret.do.auth.vat
      [~ secret]
    :_  +>.$
    :_  ~
    %+  httpreq  /auth-do
    :^    ~[%digitalocean %cloud]  `path`/v1/oauth/token
        [%post ~]
    :-  ~  `quay`['client_secret'^secret (auth-queries (need authc.do.auth.vat))]
  ==
::
++  receive-auth
  |=  [pour-path=cord resp=httr]
  ^-  [(list move) _+>.$]
  ~|  resp
  =+  body=(rash q:(need r.resp) apex:poja)
  ~|  recieve-auth/resp(r body)
  ?+  pour-path  !!
    %auth-do
  =+  [ac re]=(need ((ot 'access_token'^so 'refresh_token'^so ~):jo body))
  =:  access.do.toke.vat   ac
      refresh.do.toke.vat  re
    == 
  :_  +>.$
  :~  list-instances-do
      (publish [%lin & 'successfully authenticated']~)
  ==
  ==
::
++  poke-json
  |=  act=json
  ^-  [(list move) _+>.$]
  =+  do=(need ((ot action/so ~):jo act))
  :_  +>.$
  :_  ~
  ?+  do  !!
      %list
    list-instances-do
  ::
      %create-do
    (create-do act)
  ::
      %create-gce
    (create-gce-disk act)
  ::
    ?(%start %stop %reboot %delete)
    =+  id=(need ((ot id/so ~):jo act))
    (instance-action id do)
  ==
::
++  instance-action
  |=  $:  id=@t
      $=  action  $?  
      %start  %stop  %reboot  %delete
      ==  ==
  =+  d=(~(got by insts.vat) id)
  ~|  'can\'t find id'
  =+  typ=?~(d !! -.d)
  ?-  typ
    %do
  =+  meth=?:(?=(%delete action) %delt [%post (jobe type/s/(convert-do action) ~)])
  ^-  move
  ~&  'do i get here?'
  =+  ^=  req
  %-  httpreq  :*
     /action-test
    ~[%digitalocean %api]  
    ?:(?=(%delt meth) /v2/droplets/[id] /v2/droplets/[id]/actions)
    meth
    %^  mo  ['Content-Type' 'application/json' ~]
    ['Authorization' (cat 3 'Bearer ' access.do.toke.vat) ~]  ~
    *quay
   ==
   req
    %gce
      ?-  action
        %start
        !!
        %stop
        !!
        %reboot
        !!
        %delete
        !!
    ==
  ==
::
++  create-do
  |=  act=json
  =+  ^-  deets=create-req-do
      %-  need
      %.  act
      =>  jo
      %-  ot
      :~  name/so  size/so  image/so ::  id key:img object  region/so  
      ssh/(ar so)  backups/(mu bo)
      'ipv6'^(mu bo)  'priv_networking'^(mu bo)  'user_data'^(mu so)
      ==
  =+  ^-  body=json
      %-  create-do-body  :*
        name.deets  size.deets  image.deets  ssh.deets  backups.deets  ::region.deets  
        ipv6.deets  private-networking.deets  user-data.deets 
      ==
  %-  httpreq  :*
    /create-do
    ~[%digitalocean %api]  /v2/droplets
    [%post body]
    %^  mo  ['Content-Type' 'application/json; charset=utf-8' ~]
      ['Authorization' (cat 3 'Bearer ' access.do.toke.vat) ~]
    ~
    ~
  ==
::
++  create-gce-disk
  |=  act=json :: num=(unit ,@u)
  ~&  act
  =+  :-  name=(need ((ot name/so ~):jo act))
      snapshot=(need ((ot 'instance_img'^so ~):jo act))
  =+  :-  name=(need ((ot name/so ~):jo act))
  snap=(need ((ot snap/so ~):jo act))
  =+  ^-  body=json
      (jobe name/s/name %'sourceSnapshot'^s/'compute/v1/projects/urbcloud/global/snapshots/snapshot-1' ~)  ::^so/snap ~)
  %-  httpreq
  :*  /create-gce-disk/snapshot/name
      ~['googleapis' 'www']    /compute/v1/projects/urbcloud/zones/us-central1-a/disks
      [%post body]
     %^  mo  ['Content-Type' 'application/json' ~]
       ['Authorization' (cat 3 'Bearer ' access.gce.toke.vat) ~]
      ~
      ~ 
  ==
::
++  ask-disk-status
  |=  pax=path  ^-  move
  ~&  'ask disk status'
  =+  :-  safe=(slav %uv ?~(pax !! -.pax))
      snap=?.(?=([* ^] pax) !! i.t.pax)
  =+  link=(need (epur ?~(pax !! safe)))
  =.  r.link  ['access_token'^access.gce.toke.vat r.link]
  :^  ost  %them  `wire`/disk-status/snap
  `(unit hiss)`[~ [link [%get ~ ~]]]
::
++  disk-status                       ::receive
  |=  [ins-img=@t resp=httr]
  ^-  [(list move) _+>.$]
  ~&  'disk status called'
  =+  hcode=p.resp
  ?:  =('200' hcode)
    ~|  'did not receive 200'  !!
  =+  :-(parsed=(rash q:(need r.resp) apex:poja) jo)
  ~&  parsed
  =+  :-  status=(need ((ot status/so ~) parsed))
  lin=(need ((ot 'selfLink'^so ~) parsed))
  =+  link=(scot %uv lin)
  ?:  =('DONE' status)
    ~&  resp
    ~&  'boot disk now running, now starting instance'
    =+  target=(need ((ot 'targetLink'^so ~):jo parsed))
    =+  nam=-:(flop q.q:(need (epur target)))
    ~&  nam
    :_   +>.$  ~[(create-gce nam ins-img)]
  :_  +>.$
  [ost %wait `path`[%check-status link ins-img ~] `@da`(add ~s3 now)]~       ::  refesh every 10 sec
::
++  create-gce
  |=  [name=@t snap=@t]
  ~&  create-gce-received/snap
  =+  src=(cat 3 'compute/v1/projects/urbcloud/zones/us-central1-a/disks/' name)
  =+  ^-  body=json
      %-  jobe
      :~  name/s/name  'machineType'^s/'zones/us-central1-a/machineTypes/n1-standard-1'
      :-  %disks  :-  %a  :_  ~
      (jobe boot/b/%.y type/s/'persistent' source/s/src ~)
      :-  'networkInterfaces'  :-  %a  :_  ~
      (joba 'network' `json`[%s 'global/networks/default'])
      ==
  %-  httpreq
  :*  `path`/create-gce
      `(list cord)`~['googleapis' 'www']  `path`/compute/v1/projects/urbcloud/zones/us-central1-a/'instances'
      [%post `json`body]
     %^  mo  ['Content-Type' 'application/json' ~]
       ['Authorization' (cat 3 'Bearer ' access.gce.toke.vat) ~]
    ~
      `quay`[%key access.gce.toke.vat]~
  ==
:: 
++  wake
  |=  [pour-path=path ~]
  ?+    -.pour-path  !!
      %refresh-do
    :_  +>.$
    [list-instances-do]~
      %refresh-gce
    :_  +>.$
    [list-instances-gce]~
      %check-status
    :_  +>.$
    [(ask-disk-status +.pour-path)]~
  ==
::
++  list-instances-gce
  =+  ^=  lis
      :*
      /list-gce
      ~[%googleapis %www]  /compute/v1/projects/urbcloud/zones/['us-central1-a']/'instances'
      %get                    ~
      ^-  quay
      [%'access_token' access.gce.toke.vat]~
      ==
  (httpreq lis)
::
++  receive-list-gce
  |=  resp=httr
  ^-  [(list move) _+>.$]
  =+  parsed=(rash q:(need r.resp) apex:poja)           ::  body httr to json
  =+  items=(need ((ot items/(ar some) ~):jo parsed))
  ~&  why-no-work/resp
  =+  ^=  ins  ::^-  ins=(list ,[@t instance])
      ~|  'bad-json'^items
      %+  turn  items
      |=  in=json
      ::=<  [id .]
      ::^-  instance
      :-  %gce
      %-  need
      %.  in  =+  jo
      %-  ot
      :~  name/so  id/so  status/so  'creationTimestamp'^(su parse-iso8601)  ::zone/so
          'machineType'^(cu tail-url so)
::          'networkInterfaces'^parse-ip-gce
      ==
      ~&  ins
      :_  +>.$  ~
::  =+  ^=  new 
::      %+  skip  ins
::      |=(a=[@t instance] (~(has by insts.vat) id.a))
::  =.  insts.vat
::  (~(gas by insts.vat) new)
::  =+  buf=`@da`(add ~s10 now)
::  :_  +>.$
::  =+  lis=(~(tap by insts.vat))
::  :_  (spam (state-to-json (turn lis |=(a=[@t instance] +.a))))
::  [ost %wait /refresh-gce buf]
::
++  list-instances-do
  =+  ^=  lis
      :~  /list-do
      ~[%digitalocean %api]  /v2/droplets
      %get
      (mo ['Content-Type' 'application/json' ~] ['Authorization' (cat 3 'Bearer ' access.do.toke.vat) ~] ~)
      ==
    (httpreq lis)
::
++  receive-list-do
  |=  resp=httr
  ^-  [(list move) _+>.$]
  =+  parsed=(rash q:(need r.resp) apex:poja)           ::  parse httr to json
  ~|  recieve-list/parsed
  =+  dar=(need ((ot droplets/(ar some) ~):jo parsed))  ::  reparse ar of insts
  =.  insts.vat
    %-  ~(gas by insts.vat)
    ^-  dropz=(list ,[@t instance])
      ~|  bad-json/-.dar
      %+  turn  dar
      |=  drp=json    ^-  [@t instance]
      =-  ~!  -  -
      =<  [id .]
      ^-  instance
      :-  %do
      %-  need
      %.  drp
      =+  jo
      %-  ot
      :~  name/so  id/parse-id-text  status/so  'created_at'^(su parse-iso8601)  ::region/parse-region
          image/(ot name/so ~)  ::disk/ni  
          networks/parse-ip-do
      ==
  =+  buf=`@da`(add ~s10 now)
  :_  +>.$
  =+  lis=(~(tap by insts.vat) *(list ,[@t instance]))
  :_  (spam (state-to-json (turn lis |=(a=[@t instance] +.a))))
  [ost %wait /refresh-do buf]

++  thou
  |=  [pour-path=path resp=httr]
  ^-  [(list move) _+>.$]
  ?+    -.pour-path  ~&  pour-path  !!
      %auth-do
    (receive-auth -.pour-path resp)
  ::
      %auth-gce
    (receive-auth -.pour-path resp)
  ::
      %list-do
    (receive-list-do resp)
      %list-gce
    (receive-list-gce resp)
  ::
      $?
      %delete  %reboot  %'power_cycle'  %shutdown  %'power_off'
      %'power_on'  %'password_reset'  %'enable_ipv6'  %'enable_private_networking'
      %snapshot  %upgrade             ::  add retrieve droplet action
      %create-do  %create-gce  %action-test
      ==
      ~&  resp
      :_  +>.$  ~
  ::
      ?(%create-gce-disk %disk-status)
      =+  snap=?~(t.pour-path !! i.t.pour-path)
      ~&  snap/snap
    (disk-status snap resp)
  ::
      %check-status
    :_  +>.$  ~[(ask-disk-status +.pour-path)]
  ::
      %pub
    :_  +>.$  ~
  ::
  ==
++  publish
  |=  [act=(list speech)]
  ^-  move
  =+  ^=  spchz
      %+  turn  act
      |=  sp=speech
      =+  ^=  tail
      :-  ^-  audience
          :+  :-  `partner`[%& our ?+((clan our) !! %czar %court, %duke %porch)]
              ^-  (pair envelope delivery)
              [`envelope`[& ~] %pending]
            ~
          ~
      `statement`[now ~ sp]
      ^-  thought
      :-  `@`(sham eny tail)
      tail
  =+  mez=[%talk-command [%publish `(list thought)`spchz]]
  [ost %send /pub [our %talk] %poke mez]
--
