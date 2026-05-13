::  perm-man: 💇‍♂️
::
|%
+$  card  card:agent:gall
+$  bond  bond:ward:clay
+$  writ  writ:ward:clay
--
::
::NOTE  =, ward:clay here makes the below not be a proper $agent-gall ??????
::
^-  agent:gall
|_  =bowl:gall
+*  this  .
++  on-init  [~ this]  ::TODO  %connect
++  on-save  !>(~)
++  on-load  |=(* [~ this])
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?>  ?=(%handle-http-request mark)
  =+  !<([rid=@ta inbound-request:eyre] vase)
  =;  [caz=(list card) pay=simple-payload:http]
    =/  =path  /http-response/[rid]
    :_  this
    :*  [%give %fact ~[path] [%http-response-header !>(response-header.pay)]]
        [%give %fact ~[path] [%http-response-data !>(data.pay)]]
        [%give %kick ~[path] ~]
        caz
    ==
  ::
  =+  ^-  [[ext=(unit @ta) site=(list @t)] args=(list [key=@t value=@t])]
    =-  (fall - [[~ ~] ~])
    (rush url.request ;~(plug apat:de-purl:html yque:de-purl:html))
  =.  site
    ?>  ?=([%perm-man *] site)
    t.site
  ::
  ?+  site  [~ [404 ~] ~]
      [%page ~]
    :-  ~
    ?.  ?=(%'GET' method.request)  [[405 ~] `(as-octs:mimes:html 'bad method')]
    ::  first, get all desk state
    ::TODO  also want to know desk's liveness
    ::
    =/  desks=(map desk bond)
      =/  make-path
        |=  [=desk =spur]
        (weld /(scot %p our.bowl)/[desk]/(scot %da now.bowl) spur)
      ~>  %bout.[0 'scrying all desks']
      %+  roll
        ~(tap in .^((set desk) %cd (make-path %$ /)))
      |=  [=desk desks=(map desk bond)]
      =+  .^(=bond %cx (make-path %$ /bond/[desk]))
      ::  skip desks without any perms at all
      ::
      ?:  ?=([~ ~ ~ ~] bond)  desks
      (~(put by desks) desk bond)
    ::
    =/  attack  ::  maybe attach attribute
      |=  [at=$@(@t [nom=@t val=@t]) do=?]
      ?.  do  same
      |=  manx
      [[n.g [?@(at [at ""] [nom.at (trip val.at)]) a.g]] c]
    ::
    :-  [200 ['content-type' 'text/html']~]
    %-  some
    %-  as-octt:mimes:html
    %-  en-xml:html
    ;html
      ;head
        ;meta(charset "utf-8");
        ;title:"aa"
        ;style:"h2 \{ display: inline-block; } details \{ border: 1px solid grey; margin: 1em; }"
      ==
      ;body
        ;h1:"Perm-ission Man-agar 💇‍♂️"
        ;form(method "post", action "action")
          ;button(type "submit", value "aaa"):"save"
          ;*  %+  turn
            ::  sort to put blocking > requested > more perms > less perms
            ::
            %+  sort  ~(tap by desks)
            |=  [a=[=desk bond] b=[=desk bond]]
            ?:  ?=(^ pew.a)  &
            ?:  ?=(^ pew.b)  |
            ?:  ?=(^ peq.a)  &
            ?:  ?=(^ peq.b)  |
            (gth ~(wyt in peg.a) ~(wyt in peg.b))
          |=  [=desk bond]
          ~&  +<
          ^-  manx
          %-  (attack 'open' ?=(^ pew))
          ;details
            ;summary
              ;h2:"{(trip desk)}"  ::TODO  mb icons for status
            ==
            ;*  =-  (murn - same)
            |^  ^-  (list (unit manx))
                =/  ned=(set perm:gall)  (~(dif in ped) peg)  ::  required miss
                =/  peq=(set perm:gall)  (~(dif in peq) peg)  ::  optional miss
                =/  gor=(set perm:gall)  (~(dif in peg) ped)  ::  granted opt.
                =/  gar=(set perm:gall)  (~(dif in peg) gor)  ::  granted req.
                :~  ?~  ned  ~  %-  some
                    (render-perms "⚠️ blocking install/live" & | ned ~)
                  ::
                    ?^  ned  ~
                    ?~  pew  ~  %-  some
                    (render-perms "⚠️ blocking upgrade" & | pew ~)
                  ::
                    ?~  peq  ~  %-  some
                    (render-perms "❓ requested permissions" | | peq ~)
                  ::
                    ?:  &(=(~ gor) =(~ gar))  ~  %-  some
                    (render-perms "granted permissions" | & gor gar)
                ==
            ::
            ++  render-perms
              |=  [nom=tape open=? granted=? pes=(set perm:gall) fixed=(set perm:gall)]
              ^-  manx
              %-  (attack 'open' open)
              ;details
                ;summary:"{nom}"
                ;*  (turn ~(tap in pes) (cury (cury render-perm granted) |))
                ::TODO  not disabled if not live
                ;*  (turn ~(tap in fixed) (cury (cury render-perm granted) &))
              ==
            ::
            ++  render-perm
              |=  [granted=? disabled=? =perm:gall]
              ^-  manx
              ~?  ?=([%ames ~] perm)  [%ames-perm granted=granted disabled=disabled]
              =/  nom=tape  "{(trip desk)}/{(scow %uw (jam perm))}"  ::REVIEW
              ;label(for nom)
                ;*  ^-  marl
                :*  ^-  manx
                    :_  ~
                    :-  %input
                    =;  ats=(list (unit [mane tape]))
                      (murn ats same)
                    :~  `[`@tas`%type "checkbox"]
                        `[`@tas`%name "grant-{nom}"]
                        `[`@tas`%value "grant"]
                        ?:(granted `[`@tas`%checked ""] ~)
                        ?:(disabled `[`@tas`%disabled ""] ~)
                    ==
                  ::
                    ;input(type "hidden", name "perm-{nom}", value "{?:(granted "was-granted" "was-not-granted")}");
                  ::
                    ::NOTE  hack because checkboxes don't support "readonly".
                    ::      we do want it as enabled in form submission.
                    ;*  ?.  &(disabled granted)  ~  :_  ~
                    ;input(type "hidden", name "grant-{nom}", value "grant");
                ==
              ::
                ;+  (perm-text perm)
                ;br;
              ==
            ::
            ++  perm-text
              |=  =perm:gall
              ^-  manx
              =;  txt=tape  ;span:"{txt}"
              ?+  -.perm  "do {(trip -.perm)} thing"
                %write  ?~  dude.perm  "poke any local agent"
                        "poke {(trip dude.perm)} agent"
                %watch  ?~  dude.perm  "subscribe to any local agent for {(spud path.perm)}"
                        "subscribe to {(trip dude.perm)} agent for {(spud path.perm)}"
              ::
                  %clay
                ?-  +<.perm
                  %write  "write files {(dash-text +>.perm)}"
                  %local  "read local files {(cash-text +>.perm)}"
                  %peers  "read remote files {(cash-text +>.perm)}"
                ==
              ==
            ::
            ++  cash-text
              |=  =cash:gall
              =-  (weld - ' ' (dash-text +.cash))
              ^-  tape
              ?~  care.cash  "anything"
              ?+  u.care.cash  "specific things"
                %x  "data"
                %y  "data listing"
                %u  "existence"
                ::TODO
              ==
            ::
            ++  dash-text
              |=  dash:gall
              ^-  tape
              ?~  desk  "in any desk at {(spud spur)}"
              "in %{(trip u.desk)} desk at {(spud spur)}"
            --
          ==
        ==
      ==
    ==
  ::
      [%action ~]
    ?.  ?=(%'POST' method.request)  [~ [405 ~] `(as-octs:mimes:html 'bad method')]
    ?~  body.request                [~ [405 ~] `(as-octs:mimes:html 'bad body')]
    =;  [del-perms=(jug desk perm:gall) add-perms=(jug desk perm:gall)]
      :_  :-  [303 ['location' 'page']~]
          `(as-octs:mimes:html '🔄 applying permission... will redirect when done...')
      %+  weld
        %+  turn  ~(tap by del-perms)
        |=  [=desk pez=(set perm:gall)]
        ^-  card:agent:gall  ::TODO  normal task
        [%pass /set-seal %arvo %syscall %c %seal desk add=| pez]
      %+  turn  ~(tap by add-perms)
      |=  [=desk pez=(set perm:gall)]
      ^-  card:agent:gall  ::TODO  normal task
      [%pass /set-seal %arvo %syscall %c %seal desk add=& pez]
    =/  args=(map @t @t)
      %-  ~(gas by *(map @t @t))
      (fall (rush q.u.body.request yquy:de-purl:html) ~)
    %+  roll  ~(tap by args)
    =/  key-type  $:(kind=?(%perm %grant) =desk =perm:gall)
    =/  parse-key
      ;~  plug
        ;~(sfix (perk %perm %grant ~) hep)
        sym
        %+  sear
          |=(=@uw ((soft perm:gall) (cue uw)))
        ;~(pfix fas (jest '0w') wiz:ag)
      ==
    |=  $:  [key=@t value=@t]
            [del-perms=(jug desk perm:gall) add-perms=(jug desk perm:gall)]
        ==
    =*  next  [del-perms add-perms]
    ?~  kay=`(unit key-type)`(rush key parse-key)
      ~|(%post-parse-failed !!)
    ?.  ?=(%perm kind.u.kay)  next
    =/  was-granted=?  =(value 'was-granted')
    =/  now-granted=?  (~(has by args) (cat 3 'grant' (rsh 3^4 key)))
    ?:  =(was-granted now-granted)  next
    ?:  now-granted
      ~&  [%adding +.u.kay]
      [del-perms (~(put ju add-perms) +.u.kay)]
    ~&  [%deleting +.u.kay]
    [(~(put ju del-perms) +.u.kay) add-perms]
  ==
::
++  on-watch
  |=  =path
  ?>  ?=([%http-response @ ~] path)
  [~ this]
::
++  on-leave  |=(* [~ this])
++  on-agent  |=(* [~ this])
++  on-arvo   |=(* [~ this])  ::TODO  mb receive wards?
::
++  on-fail
  |=  [=term =tang]
  %.  [~ this]
  (slog (rap 3 dap.bowl ': on-fail: ' term ~) tang)
::
++  on-peek   |=(path `(unit (unit cage))`~)
--
