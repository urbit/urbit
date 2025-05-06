::  perms: permission categorization and rendering
::
::    turns permission nouns into strings and sorts them into buckets.
::    the $passport structure here is intended for consumption by permission
::    management frontends.
::
::TODO  do wo need additional args to bucketize "internal" permissions for to-be-installed apps correctly?
::
::
=,  gall
|%
+$  visa  [deal =pers:gall have=?(%all %any %nil)]
+$  deal
  $:  kind=@t
      info=@t
      warn=(unit @t)
      tags=(set tag)
  ==
+$  tag
  ?(%local %remote %read %write %kernel %core %app %http)
::
+$  passport                                          ::  categorized perms:
  $:  rad=perm-many                                   ::  dangerous perms
      sys=perm-many                                   ::  system perms
      any=perm-many                                   ::  all apps perms
      new=perm-many                                   ::  unknown app perms
      app=(list [app=@t pes=perm-many])               ::  specific app perms
  ==
::
+$  perm-many  (list perm-once)
+$  perm-once
  $%  [%node perm-node]                               ::  lowest-level perm
      [%kind nom=@t pes=(set perm-node)]              ::  nested in category
  ==
::
+$  perm-node
  $:  desc=@t
      warn=(unit @t)
      have=?(%all %any %nil)
      pers=pers
  ==
::
++  scry-live
  |=  [our=@p now=@da]
  ^-  (map dude desk)
  =/  our  (scot %p our)
  =/  now  (scot %da now)
  %-  %~  rep  in
      .^((set desk) %cd /[our]//[now])
  |=  [des=desk out=(map dude desk)]
  %-  %~  rep  in
      .^((set [=dude live=?]) %ge /[our]/[des]/[now]/$)
  |=  [[dud=dude ?] =_out]
  (~(put by out) dud des)
::
++  scry-approved
  |=  [our=@p now=@da =desk]
  ^-  pers:gall
  =+  .^(=cone:clay %cx /(scot %p our)//(scot %da now)/domes)
  pes:(~(gut by cone) [our desk] *dome:clay)
::
++  perm-tree
  |=  $:  live=(map dude desk)
          apps=(map desk @t)
          last=pers:gall
      ==
  |^  |=  =pers
      (perm-tree pers)
  ::
  ++  perm-visa
    |=  =pers
    ^-  (list visa)
    =-  (sort - aor)
    %+  turn
      %~  tap  by
      %+  roll  ~(tap in pers)
      |=  [=perm pers=(jug deal perm:gall)]
      =/  =deal
        [(perm-kind perm) (perm-cord perm) (perm-warn perm) (perm-tags perm)]
      (~(put ju pers) deal perm)
    |=  [=deal =pers:gall]
    :+  deal  pers
    ?~  (~(int in pers) last)  %nil
    ?~  (~(dif in pers) last)  %all
    %any
  ::
  ++  perm-tree
    |=  =pers
    ^-  passport
    ::  group perms into the smallest possible groupings
    ::
    =/  j=(jug deet perm)
      %-  ~(rep in pers)
      |=  [p=perm j=(jug deet perm)]
      ^+  j
      =-  (~(put ju j) - p)
      [(perm-warn p) (perm-pail p) (perm-kind p) (perm-cord p)]
    ::  sort groupings by kind
    ::
    =/  k=[nodes=(list [buck perm-once]) kinds=(jug [buck kind=@t] perm-node)]
      %-  ~(rep by j)
      |=  $:  [deet =^pers]
              nodes=(list [buck perm-once])
              kinds=(jug [buck @t] perm-node)
          ==
      ^+  [nodes kinds]
      =/  haves=?(%all %any %nil)
        ?~  (~(int in pers) last)  %nil
        ?~  (~(dif in pers) last)  %all
        %any
      ?:  =(kind desc)
        :_  kinds
        [[buck %node desc warn haves pers] nodes]
      :-  nodes
      (~(put ju kinds) [buck kind] [desc warn haves pers])
    ::  sort groupings by bucket
    ::
    =/  m=(jar buck perm-once)
      %+  roll  nodes.k
      |=  [[b=buck p=perm-once] m=(jar buck perm-once)]
      (~(add ja m) b p)
    =.  m
      %-  ~(rep by kinds.k)
      |=  [[[b=buck k=@t] p=(set perm-node)] =_m]
      (~(add ja m) b [%kind k p])
    ::  build the passport
    ::
    :*  (~(get ja m) %rad)
        (~(get ja m) %sys)
        (~(get ja m) %any)
        (~(get ja m) %new)
      ::
        %+  murn  ~(tap by m)
        |=  [b=buck p=perm-many]
        ^-  (unit [app=@t perm-many])
        ?:(?=([%app *] b) `[+.b p] ~)
    ==
  ::
  +$  buck  ?(%sys %rad %new %any [%app @t])
  +$  deet  [warn=(unit @t) =buck kind=@t desc=@t]
  ::
  ++  from-desk
    |=  =desk
    ^-  @t
    ?~  a=(~(get by apps) desk)
      (cat 3 'unknown app %' desk)
    ?.  =('' u.a)  u.a
    (cat 3 '%' desk)
  ::
  ++  from-dude
    |=  [deet=? =dude]
    ^-  @t
    ?~  a=(~(get by live) dude)
      (cat 3 'unknown agent %' dude)
    ?.  =(%base u.a)  (from-desk u.a)
    ?.  deet  'the os'
    (cat 3 '%' dude)
  ::
  ++  with-desk
    |=  [=cord =desk]
    (cat 3 cord (from-desk desk))
  ::
  ++  with-dude
    |=  [deet=? =cord =dude]
    (cat 3 cord (from-dude deet dude))
  ::
  ++  perm-warn  ::  "this may let the app ..."
    |=  =perm
    ^-  (unit @t)
    =*  full  `'take full control of your urbit'
    ?+  perm  ~
      [%write * ~]            full
      [%write * %hood]        full
      [%write * %spider]      full
      [%super ~]              full
      [%clay %creds ~]        `'expose your files to the public'
      [%clay %write ~ *]      full
      [%clay %write [~ %base] *]  full
      [%clay %write ^ *]      `(with-desk 'modify the behavior of ' u.desk.perm)
      [%clay %perms *]        full
      [%dill %input]          full
      [%eyre %serve]          full
      [%gall %clear ~]        `'wipe the data of all your apps'
      [%gall %clear ^]        `(with-desk 'wipe the data of ' u.dude.perm)
      [%iris %fetch]          full
      [%jael %prick]          `'impersonate you on the network, read all your messages'
      [%jael %break]          `'prevent you from communicating with other ships'
    ==
  ::
  ++  perm-pail  ::  category the permission belongs in
    |=  =perm
    |^  ^-  buck
        ?^  (perm-warn perm)    %rad
        ?+  perm  %sys
          [%write * ~]          %any
          [%write * @]          (from-name dude.perm)
          [%watch * ~ *]        %any
          [%watch * @tas *]     (from-name dude.perm)
          [%reads %g * ~ *]     %any
          [%reads %g * ^ *]     (from-name u.desk.perm)
        ::
          [%clay %write ~ *]    %any
          [%clay %write ^ *]    (from-desk u.desk.perm)
          [%clay %local * ~ *]  %any
          [%clay %local * ^ *]  (from-desk u.desk.perm)
          [%clay %peers * ~ *]  %any
          [%clay %peers * ^ *]  (from-desk u.desk.perm)
          [%clay %perms ~]      %any
          [%clay %perms ^]      (from-desk u.desk.perm)
          [%clay %plead ~]      %any
          [%clay %plead ^]      (from-desk u.desk.perm)
          [%clay %liven ~]      %any
          [%clay %liven ^]      (from-desk u.desk.perm)
          [%gall %clear ~]      %any
          [%gall %clear ^]      (from-name u.dude.perm)
        ==
    ::
    ++  from-name
      |=  =dude
      ^-  buck
      (fall (bind (~(get by live) dude) from-desk) %new)
    ::
    ++  from-desk
      |=  =desk
      ^-  buck
      ?:  =(%base desk)  %sys
      ?~  a=(~(get by apps) desk)  %new
      :-  %app
      ?.  =('' u.a)  u.a
      (cat 3 '%' desk)
    --
  ::
  ++  core-dudes  ^~
    ^-  (set dude:gall)
    %-  sy
    :~  %acme  %aqua  %azimuth-rpc  %azimuth-tracker  %azimuth  %claz  %dbug
        %dns-collector  %dojo  %eth-sender  %eth-watcher  %gaze  %herm  %hood
        %language-server  %lens  %ping  %roller-rpc  %roller  %shoe  %spider
        %test  %time
    ==
  ::
  ++  perm-tags
    |=  =perm
    |^  (sy tags)
    ::
    ++  with-network
      |=  [jump=? tags=(list tag)]
      [%local ?:(jump [%remote tags] tags)]
    ::
    ++  with-dude
      |=  [=dude:gall tags=(list tag)]
      ?:  (~(has in core-dudes) dude)  [%core tags]
      [%app tags]
    ::
    ++  with-desk
      |=  [=desk tags=(list tag)]
      ?:(=(%base desk) [%core tags] [%app tags])
    ::
    ++  tags
      ^-  (list tag)
      ?-  perm
        [%super ~]        ~
      ::
          [%reads *]
        :+  %local  %read
        ?+  vane.perm  [%kernel]~
          ?(%c %g)  ?~(desk.perm ~[%core %app] (with-desk u.desk.perm ~))
        ==
      ::
        [%write *]        (with-network jump.perm %write %app ~)
        [%watch *]        (with-network jump.perm %read %app ~)
        [%press *]        ~[%app %remote]
      ::
        [%ames %debug ~]  ~[%local %kernel]
        [%ames %block ~]  ~[%remote %kernel]
        [%ames %order *]  ~[%remote %app %read]
        [%ames %whack *]  ~[%remote %app %read]
        [%behn %timer]    ~[%local %kernel]
        [%clay %mount ~]  ~[%local %kernel]
        [%clay %creds ~]  ~[%local %kernel %read %write]
        [%clay %label *]  ~[%local %kernel %write]
        [%clay %write *]  ?~  desk.perm  ~[%local %write %core %app]
                          (with-desk u.desk.perm %local %write ~)
        [%clay %local *]  ?~  desk.perm  ~[%local %read %core %app]
                          (with-desk u.desk.perm %local %read ~)
        [%clay %peers *]  ~[%remote %read %app]
        [%clay %perms ~]  ~[%local %app %core]
        [%clay %perms ^]  (with-desk u.desk.perm %local ~)
        [%clay %plead ~]  ~[%local %app %core]
        [%clay %plead ^]  (with-desk u.desk.perm %local ~)
        [%clay %liven ~]  ~[%local %app %core]
        [%clay %liven ^]  (with-desk u.desk.perm %local ~)
        [%clay %pulse ~]  ~[%local %app %core]
        [%clay %grave *]  ~[%local %kernel %write]
        [%dill *]         ~[%local %kernel]
        [%eyre *]         ~[%local %kernel %http]
        [%gall %clear ~]  ~[%local %app %core]
        [%gall %clear ^]  (with-dude u.dude.perm %local ~)
        [%gall %guard ~]  ~[%kernel %local]
        [%iris %fetch]    ~[%kernel %http]
        [%jael %moons]    ~[%kernel %remote %write]
        [%jael %prick]    ~[%kernel %local %read]
        [%jael %creak]    ~[%kernel %local %write]
        [%jael %login]    ~[%kernel %local %write]
        [%jael %break]    ~[%kernel %remote]
        [%khan %tread]    ~
      ==
    --
  ::
  ++  perm-kind  ::  "this app wants to ..." but higher-level
    |=  =perm
    ^-  @t
    ?-  perm
      [%super ~]         'do everything'
      [%watch *]         ?~  dude.perm  'read data from all your apps'
                         (with-dude | 'read data from ' dude.perm)
      [%reads %g * ~ *]  'read data from all your apps'
      [%reads %g * ^ *]  ?:  =(%$ u.desk.perm)  'read data from the os'
                         (with-dude | 'read data from ' u.desk.perm)
      [%reads %c * ~ *]  'read data from all your apps'
      [%reads %c * ^ *]  ?:  =(%$ u.desk.perm)  'read data from the os'
                         (with-desk 'read data from ' u.desk.perm)
      [%reads *]         'read data from the os'
      [%write * ~]       'interact with all your apps'
      [%write * *]       (with-dude | 'interact with ' dude.perm)
      [%press *]         'publish and manage data'
    ::
      [%ames %debug ~]  'control networking logging levels'
      [%ames %block ~]  'manage the networking blocklist'
      [%ames %order *]  'read data from other urbits'
      [%ames %whack *]  'manage data requests'
      [%behn %timer]    'set and unset timers'
      [%clay %mount ~]  'manage the filesystem'
      [%clay %creds ~]  'manage the filesystem'
      [%clay %label *]  'manage the filesystem'
      [%clay %write *]  'save and modify files'
      [%clay %local *]  ?~  desk.perm  'read data from all your apps'
                        (with-desk 'read data from ' u.desk.perm)
      [%clay %peers *]  'read files from other urbits'
      [%clay %perms ~]  'manage app permissions for all apps'
      [%clay %perms ^]  (with-desk 'manage app permissions for ' u.desk.perm)
      [%clay %plead ~]  'request new permissions for all apps'
      [%clay %plead ^]  (with-desk 'request new permissions for ' u.desk.perm)
      [%clay %liven *]  'manage your apps'
      [%clay %pulse ~]  'manage your apps'
      [%clay %grave *]  'manage the filesystem'
      [%dill %views]    'use the terminal'
      [%dill %input]    'use the terminal'
      [%dill %print]    'use the terminal'
      [%dill %extra]    'manage your system'
      [%eyre %serve]    'manage the http server'
      [%eyre %certs]    'manage the http server'
      [%eyre %perms]    'manage the http server'
      [%gall %clear *]  'manage your apps'
      [%gall %guard ~]  'manage your apps'
      [%iris %fetch]    'make and cancel http requests'
      [%jael %moons]    'manage your credentials'
      [%jael %prick]    'manage your credentials'
      [%jael %creak]    'manage your credentials'
      [%jael %login]    'manage your credentials'
      [%jael %break]    'destroy connectivity with other ships'
      [%khan %tread]    'run asynchronous computation'
    ==
  ::
  ++  perm-cord  ::  "this app wants to ..."
    |=  =perm
    ^-  @t
    ?-  perm
      [%super ~]        'do everything'
      [%reads *]        (cat 3 'read ' (full-spar +.perm))
      [%write * ~]      'interact with all your apps'
      [%write * @]      (with-dude & 'interact with ' dude.perm)
      [%press *]        'publish and manage data'
    ::
        [%watch *]
      %+  rap  3
      :~  'read '
          ?:(jump.perm 'local and remote' 'local')
          ' app data from '
          ?~(dude 'all your apps' (with-dude & '' `@`dude.perm))
        ::
          ?~  path.perm  ''
          (cat 3 ' under ' (spat path.perm))
      ==
    ::
      [%ames %debug ~]  'control networking logging levels'
      [%ames %block ~]  'manage the networking blocklist'
      [%ames %order *]  'read data from other urbits'
      [%ames %whack *]  'cancel requests for data from other urbits'
      [%behn %timer]    'set and unset timers'
      [%clay %mount ~]  'control unix mountpoints'
      [%clay %creds ~]  'manage filesystem permissions'
      [%clay %label ~]  'add filesystem labels on any desk'
      [%clay %label ^]  (with-desk 'add filesystem labels on ' u.desk.perm)
    ::
        [%clay %write *]
      %+  rap  3
      :~  'write files'
          ?~  desk.perm  ''
          (with-desk ' on ' u.desk.perm)
        ::
          ?~  spur.perm  ''
          (cat 3 ' at and under ' (spat spur.perm))
      ==
    ::
      [%clay %local *]  (cat 3 'read ' (full-spar %c +>.perm))
      [%clay %peers *]  (cat 3 'read remote ' (full-spar %c +>.perm))
      [%clay %perms ~]  'manage app permissions for all apps'
      [%clay %perms ^]  (with-desk 'manage app permissions for ' u.desk.perm)
      [%clay %plead ~]  'request new permissions for all apps'
      [%clay %plead ^]  (with-desk 'request new permissions for ' u.desk.perm)
      [%clay %liven ~]  'start & suspend any apps'
      [%clay %liven ^]  (with-desk 'start & suspend ' u.desk.perm)
      [%clay %pulse ~]  'see what apps you are running'
    ::
        [%clay %grave *]
      %+  rap  3
      :~  'manage old file revision retention policy for '
          ?~  desk.perm  'all apps'
          (with-desk '' u.desk.perm)
        ::
          ?~  ship.perm  ''
          (cat 3 ' from ' (scot %p u.ship.perm))
      ==
    ::
      [%dill %views]    'view all terminal output'
      [%dill %input]    'simulate terminal input'
      [%dill %print]    'print output into the terminal'
      [%dill %extra]    'trigger system cleanup procedures'
      [%eyre %serve]    'manage http endpoints'
      [%eyre %certs]    'manage https certificates & dns bindings'
      [%eyre %perms]    'manage cors approvals'
      [%gall %clear ~]  'wipe the data of all your apps'
      [%gall %clear ^]  (with-dude & 'wipe the data of ' u.dude.perm)
      [%gall %guard ~]  'see what permissions you have configured'
      [%iris %fetch]    'make and cancel http requests'
      [%jael %moons]    'create and rekey moons'
      [%jael %prick]    'read your networking private keys'
      [%jael %creak]    'update your networking keys'
      [%jael %login]    'update your login code'
      [%jael %break]    'destroy connectivity with other ships'
      [%khan %tread]    'run asynchronous computation'
    ==
  ::
  ++  full-spar
    |=  [vane=term spar]
    ^-  @t
    ::TODO  custom strings for known scry paths, like jael keys
    %+  rap  3
    :~  ?~  care  'any data'
        ?+  vane  (rap 3 '%' u.care ' data' ~)
          %c  (care-clay u.care)
          %g  (care-gall u.care)
        ==
      ::
        ?.  ?=(?(%c %g) vane)  (cat 3 ' from ' (grow vane))
        ?-  desk
          ~       ' from all your apps'
          [~ %$]  (cat 3 'from ' (grow vane))
          [~ @]   (?-(vane %c with-desk, %g (cury with-dude &)) ' from ' u.desk)
        ==
      ::
        ?~  spur  ''
        (cat 3 ' under ' (spat spur))
    ==
  ::
  ++  care-gall
    |=  care=term
    ?+  care  'app metadata'
      %x  'app data'
      %u  'app data existence'
    ==
  ::
  ++  care-clay
    |=  care=term
    ?+  care  '[unrecognized clay datatype]'
      ?(%a %b %c %e %f)  'programs'
      :: %a  'compiled files'
      :: %b  'processed mark cores'
      :: %c  'mark conversions'
      %d  'full desk state'
      :: %e  'typed mark cores'
      %f  'casted files'
      %p  'file permissions'
      %r  'stored data'
      %s  'miscellaneous data'
      :: %t  'directory trees'
      :: %u  'file existence'
      %v  'full desk state'
      %w  'revision numbers'
      :: %y  'directory listings'
      %x  'stored data'
      :: %z  'file hashes'
      ?(%t %u %y %z)  'directory listings'
    ==
  --
--
