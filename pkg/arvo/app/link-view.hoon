::  link-view: frontend endpoints
::
::  endpoints, mapping onto link-store's paths. p is for page as in pagination.
::  updates only work for page 0.
::  as with link-store, urls are expected to use +wood encoding.
::
::    /json/[p]/submissions                           pages for all groups
::    /json/[p]/submissions/[some-group]              page for one group
::    /json/[p]/discussions/[wood-url]/[some-group]   page for url in group
::    /json/[n]/submission/[wood-url]/[some-group]    nth matching submission
::    /json/seen                                      mark-as-read updates
::
/+  *link, *server, default-agent, verb
::
|%
+$  state-0
  $:  %0
      ~
  ==
::
+$  card  card:agent:gall
--
::
=|  state-0
=*  state  -
::
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this
    :~  [%pass /connect %arvo %e %connect [~ /'~link'] dap.bowl]
        [%pass /submissions %agent [our.bowl %link-store] %watch /submissions]
        [%pass /discussions %agent [our.bowl %link-store] %watch /discussions]
        [%pass /seen %agent [our.bowl %link-store] %watch /seen]
      ::
        =+  [dap.bowl /tile '/~link/js/tile.js']
        [%pass /launch %agent [our.bowl %launch] %poke %launch-action !>(-)]
    ==
  ::
  ++  on-save  !>(state)
  ::
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    [~ this(state !<(state-0 old))]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    :_  this
    ?+  mark  (on-poke:def mark vase)
        %handle-http-request
      =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
      %+  give-simple-payload:app  eyre-id
      %+  require-authorization:app  inbound-request
      handle-http-request:do
    ::
        %link-action
      [(handle-action:do !<(action vase)) ~]
    ==
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?:  ?|  ?=([%http-response *] path)
            ?=([%json %seen ~] path)
        ==
      [~ this]
    ?:  ?=([%tile ~] path)
      :_  this
      ~[give-tile-data:do]
    ?.  ?=([%json @ @ *] path)
      (on-watch:def path)
    =/  p=@ud  (slav %ud i.t.path)
    ?+  t.t.path  (on-watch:def path)
        [%submissions ~]
      :_  this
      (give-initial-submissions:do p ~)
    ::
        [%submissions ^]
      :_  this
      (give-initial-submissions:do p t.t.t.path)
    ::
        [%submission @ ^]
      :_  this
      (give-specific-submission:do p (break-discussion-path t.t.t.path))
    ::
        [%discussions @ ^]
      :_  this
      (give-initial-discussions:do p (break-discussion-path t.t.t.path))
    ==
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+  -.sign  (on-agent:def wire sign)
        %kick
      :_  this
      [%pass wire %agent [our.bowl %link-store] %watch wire]~
    ::
        %fact
      =*  mark  p.cage.sign
      =*  vase  q.cage.sign
      ?+  mark  (on-agent:def wire sign)
        %link-initial  [~ this]
      ::
          %link-update
        :_  this
        :-  (send-update:do !<(update vase))
        ?:  =(/discussions wire)  ~
        ~[give-tile-data:do]
      ==
    ==
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?.  ?=([%e %bound *] sign-arvo)
      (on-arvo:def wire sign-arvo)
    ~?  !accepted.sign-arvo
      [dap.bowl "bind rejected!" binding.sign-arvo]
    [~ this]
  ::
  ++  on-peek   on-peek:def
  ++  on-leave  on-leave:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
++  page-size  25
++  get-paginated
  |*  [p=(unit @ud) l=(list)]
  ^-  [total=@ud pages=@ud page=_l]
  :+  (lent l)
    %+  add  (div (lent l) page-size)
    (min 1 (mod (lent l) page-size))
  ?~  p  l
  %+  scag  page-size
  %+  slag  (mul u.p page-size)
  l
::
++  page-to-json
  =,  enjs:format
  |*  $:  page-number=@ud
          [total-items=@ud total-pages=@ud page=(list)]
          item-to-json=$-(* json)
      ==
  ^-  json
  %-  pairs
  :~  'totalItems'^(numb total-items)
      'totalPages'^(numb total-pages)
      'pageNumber'^(numb page-number)
      'page'^a+(turn page item-to-json)
  ==
::
++  handle-http-request
  |=  =inbound-request:eyre
  ^-  simple-payload:http
  ?.  =(src.bowl our.bowl)
    [[403 ~] ~]
  ::  request-line: parsed url + params
  ::
  =/  =request-line
    %-  parse-request-line
    url.request.inbound-request
  =*  req-head  header-list.request.inbound-request
  ?+  method.request.inbound-request  not-found:gen
      %'GET'
    (handle-get req-head request-line)
  ==
::
++  handle-get
  |=  [request-headers=header-list:http =request-line]
  ^-  simple-payload:http
  ::  try to load file from clay
  ::
  ?~  ext.request-line
    ::  for extension-less requests, always just serve the index.html.
    ::  that way the js can load and figure out how to deal with that route.
    ::
    $(request-line [[`%html ~[%'~link' 'index']] args.request-line])
  =/  file=(unit octs)
    ?.  ?=([%'~link' *] site.request-line)  ~
    (get-file-at /app/link [t.site u.ext]:request-line)
  ?~  file  not-found:gen
  ?+  u.ext.request-line  not-found:gen
    %html  (html-response:gen u.file)
    %js    (js-response:gen u.file)
    %css   (css-response:gen u.file)
    %png   (png-response:gen u.file)
  ==
::
++  get-file-at
  |=  [base=path file=path ext=@ta]
  ^-  (unit octs)
  ::  only expose html, css and js files for now
  ::
  ?.  ?=(?(%html %css %js %png) ext)
    ~
  =/  =path
    :*  (scot %p our.bowl)
        q.byk.bowl
        (scot %da now.bowl)
        (snoc (weld base file) ext)
    ==
  ?.  .^(? %cu path)
    ~
  %-  some
  %-  as-octs:mimes:html
  .^(@ %cx path)
::
++  handle-action
  |=  =action
  ^-  card
  [%pass /action %agent [our.bowl %link-store] %poke %link-action !>(action)]
::  +give-tile-data: total unread count as json object
::
::NOTE  the full recalc of totals here probably isn't the end of the world.
::      but in case it is, well, here it is.
::
++  give-tile-data
  ^-  card
  =;  =json
    [%give %fact ~[/tile] %json !>(json)]
  %+  frond:enjs:format  'unseen'
  %-  numb:enjs:format
  %-  %~  rep  in
      (scry-for (jug path url) /unseen)
  |=  [[=path unseen=(set url)] total=@ud]
  %+  add  total
  ~(wyt in unseen)
::
::  +give-initial-submissions: page of submissions on path
::
::    for the / path, give page for every path
::
::    result is in the shape of: {
::      "/some/path": {
::        totalItems: 1,
::        totalPages: 1,
::        pageNumber: 0,
::        page: [
::          { commentCount: 1, ...restOfTheSubmission }
::        ]
::      },
::      "/maybe/more": { etc }
::    }
::
++  give-initial-submissions
  |=  [p=@ud =path]
  ^-  (list card)
  :_  ?:  =(0 p)  ~
      [%give %kick ~ ~]~
  =;  =json
    [%give %fact ~ %json !>(json)]
  %+  frond:enjs:format  'initial-submissions'
  %-  pairs:enjs:format
  %+  turn
    %~  tap  by
    %+  scry-for  (map ^path submissions)
    [%submissions path]
  |=  [=^path =submissions]
  ^-  [@t json]
  :-  (spat path)
  =;  =json
    ::  add unseen count
    ::
    ?>  ?=(%o -.json)
    :-  %o
    %+  ~(put by p.json)  'unseenCount'
    %-  numb:enjs:format
    %~  wyt  in
    %+  scry-for  (set url)
    [%unseen path]
  %^  page-to-json  p
    %+  get-paginated  `p
    submissions
  |=  =submission
  ^-  json
  =/  =json  (submission:en-json submission)
  ?>  ?=([%o *] json)
  ::  add in seen status
  ::
  =.  p.json
    %+  ~(put by p.json)  'seen'
    :-  %b
    %+  scry-for  ?
    [%seen (build-discussion-path path url.submission)]
  ::  add in comment count
  ::
  =;  comment-count=@ud
    :-  %o
    %+  ~(put by p.json)  'commentCount'
    (numb:enjs:format comment-count)
  %-  lent
  ~|  [path url.submission]
  ^-  comments
  =-  (~(got by (~(got by -) path)) url.submission)
  %+  scry-for  (per-path-url comments)
  :-  %discussions
  (build-discussion-path path url.submission)
::
++  give-specific-submission
  |=  [n=@ud =path =url]
  :_  [%give %kick ~ ~]~
  =;  =json
    [%give %fact ~ %json !>(json)]
  %+  frond:enjs:format  'submission'
  ^-  json
  =;  sub=(unit submission)
    ?~  sub  ~
    (submission:en-json u.sub)
  =/  =submissions
    =-  (~(got by -) path)
    %+  scry-for  (map ^path submissions)
    [%submissions path]
  |-
  ?~  submissions  ~
  =*  sub  i.submissions
  ?.  =(url.sub url)
    $(submissions t.submissions)
  ?:  =(0 n)  `sub
  $(n (dec n), submissions t.submissions)
::
++  give-initial-discussions
  |=  [p=@ud =path =url]
  ^-  (list card)
  :_  ?:  =(0 p)  ~
      [%give %kick ~ ~]~
  =;  =json
    [%give %fact ~ %json !>(json)]
  %+  frond:enjs:format  'initial-discussions'
  %^  page-to-json  p
    %+  get-paginated  `p
    =-  (~(got by (~(got by -) path)) url)
    %+  scry-for  (per-path-url comments)
    [%discussions (build-discussion-path path url)]
  comment:en-json
::
++  send-update
  |=  =update
  ^-  card
  ?+  -.update  ~|([dap.bowl %unexpected-update -.update] !!)
      %submissions
    %+  give-json
      (update:en-json update)
    :~  /json/0/submissions
        (weld /json/0/submissions path.update)
    ==
  ::
      %discussions
    %+  give-json
      (update:en-json update)
    :_  ~
    %+  weld  /json/0/discussions
    (build-discussion-path [path url]:update)
  ::
      %observation
    %+  give-json
      (update:en-json update)
    ~[/json/seen]
  ==
::
++  give-json
  |=  [=json paths=(list path)]
  ^-  card
  [%give %fact paths %json !>(json)]
::
++  scry-for
  |*  [=mold =path]
  .^  mold
    %gx
    (scot %p our.bowl)
    %link-store
    (scot %da now.bowl)
    (snoc `^path`path %noun)
  ==
--
