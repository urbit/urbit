::  perm-man: 💇‍♂️
::
|%
+$  card   card:agent:gall
+$  bond   bond:ward:clay
+$  writ   writ:ward:clay
+$  rock   rock:tire:clay
--
::
::NOTE  =, ward:clay here makes the below not be a proper $agent-gall ??????
::
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this
    [%pass /connect %arvo %eyre %connect [~ /'perm-man'] dap.bowl]~
  ::
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
    =/  make-path
        |=  [=desk =spur]
        (weld /(scot %p our.bowl)/[desk]/(scot %da now.bowl) spur)
    ::
    =/  bad-method  [[405 ~] `(as-octs:mimes:html 'bad method')]
    =/  bad-body    [[405 ~] `(as-octs:mimes:html 'bad body')]
    =/  parse-fail  [[400 ~] `(as-octs:mimes:html 'fail to parse')]
    ::
    ?+  site  [~ [404 ~] ~]
        [%page ~]
      :-  ~
      ?.  ?=(%'GET' method.request)  bad-method
      ::~>  %bout.[0 'scrying all desks']
      =/  build  ~(. build bowl)
      ::
      :-  [200 ['content-type' 'text/html']~]
      %-  some
      %-  as-octt:mimes:html
      %-  en-xml:html
      page:build
    ::
        [%desk @ ~]
      :-  ~
      ?.  ?=(%'GET' method.request)                 bad-method
      =/  target=desk      +<.site
      ?.  .^(? %cu (make-path target /sys/kelvin))  [[204 ~] ~]
      =/  build  ~(. build bowl)
      =+  .^(=cone:clay %cx (make-path %$ /domes))
      =/  data=[desk =mod-bond:build =zest:clay ship (set weft) ese=? inst=?]
        (get-desk-data:build cone target)
      ?.  inst.data                                 [[204 ~] ~]
      ::  ??
      =/  status=zest:clay
        ::  if held for pew reasons (not just wic reasons),
        ::  mark status as "dead", action required
        ::
        ?:  &(?=(%held zest.data) ?=(^ pew.mod-bond.data))  %dead
        zest.data
      :-  [200 ['content-type' 'text/html'] ['install-status' status]~]
      `(as-octt:mimes:html (en-xml:html (display:build data)))
    ::
        [%action @ ~]
      ?.  ?=(%'POST' method.request)  [~ bad-method]
      ?~  body.request                [~ bad-body]
      =;  [del-perms=(jug desk perm:gall) add-perms=(jug desk perm:gall)]
        =/  redirect
          (crip "/perm-man/page#{(trip +<.site)}")
        :_  :-  [303 'location'^redirect ~]
            `(as-octs:mimes:html '🔄 applying permission... will redirect when done...')
        %+  weld
          %+  turn  ~(tap by del-perms)
          |=  [=desk pez=(set perm:gall)]
          ^-  card:agent:gall
          [%pass /set-seal %arvo %clay %seal desk add=| pez]
        %+  turn  ~(tap by add-perms)
        |=  [=desk pez=(set perm:gall)]
        ^-  card:agent:gall
        [%pass /set-seal %arvo %clay %seal desk add=& pez]
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
        ~&  >>  [%adding +.u.kay]
        [del-perms (~(put ju add-perms) +.u.kay)]
      ~&  >>  [%deleting +.u.kay]
      [(~(put ju del-perms) +.u.kay) add-perms]
    ::
        ?([%submit-prompt ~] [%submit-prompt @ ~])
      ?.  ?=(%'POST' method.request)  [~ bad-method]
      ?~  body.request                [~ bad-body]
      =/  args=(map @t @t)
        %-  ~(gas by *(map @t @t))
        (fall (rush q.u.body.request yquy:de-purl:html) ~)
      ~&  >  args=args
      =/  redirect  ?.  ?=([%submit-prompt @ ~] site)  'page'
                    (crip "/perm-man/page#{(trip +<.site)}")
      =;  add=(jug desk perm:gall)
        ~&  >>  add=add
        :_  :-  [303 'location'^redirect ~]
            `(as-octs:mimes:html '🔄 applying permission... will redirect when done...')
        %+  turn  ~(tap by add)
        |=  [=desk pez=(set perm:gall)]
        ^-  card:agent:gall
        [%pass /set-seal %arvo %clay %seal desk add=& pez]
      %+  roll  ~(tap by args)
      |=  [[key=@t value=@t] add=(jug desk perm:gall)]
      ?.  =('decision-' (end 3^9 key))  add
      ?.  =('allow' value)  add
      =/  =desk  ;;(desk (rsh 3^9 key))
      ?~  blob=(~(get by args) (cat 3 'perms-' desk))  add
      =/  pez  ;;((set perm:gall) (cue (slav %uw u.blob)))
      (~(gas ju add) (turn ~(tap in pez) |=(p=perm:gall [desk p])))
    ::
        [%install ~]
      ?.  ?=(%'POST' method.request)  [~ bad-method]
      ?~  body.request                [~ bad-body]
      =/  dat=(unit [ship desk])
        %+  rush  q.u.body.request
        ;~(plug ;~(pfix sig fed:ag) ;~(pfix fas sym))
      ?~  dat  [~ [405 ~] `(as-octs:mimes:html 'fail to parse')]  ::TODO
      ~&  dat=u.dat
      =/  [her=ship =desk]  u.dat
      =/  redirect  (crip "page#{(trip desk)}")
      :_  :-  [303 'location'^redirect ~]
            `(as-octs:mimes:html (crip "🔄 beggining {<-.u.dat>} installation..redirect when started"))
      [%pass /install/[desk] %agent [our.bowl %hood] %poke %kiln-install !>([desk her desk])]~
    ::
        [%suspend ~]
      ?.  ?=(%'POST' method.request)  [~ bad-method]
      ?~  body.request                [~ bad-body]
      =/  args=(map @t @t)
        %-  ~(gas by *(map @t @t))
        (fall (rush q.u.body.request yquy:de-purl:html) ~)
      =/  des=(list desk)
        ?:  (~(has by args) 'suspend')
          [(~(got by args) 'suspend') ~]
        %+  murn  ~(tap by (~(del by args) 'suspend-all'))
        |=  [key=@t value=@t]
        ^-  (unit desk)
        ?.  =('allow' value)  ~
        (rush key ;~(pfix (jest 'suspend-') sym))
      :_  :-  [303 'location'^(crip "page#base") ~]
            `(as-octs:mimes:html (crip "🔄 setting to non-essential {<des>}..redirect when started"))
      ::  setting desks to non-essential when
      %+  turn  des
      |=  =desk
      ^-  card
      [%pass /non-esse/[desk] %arvo %clay %esse desk |]
    ==
  ::
  ++  on-watch
    |=  =path
    ?>  ?=([%http-response @ ~] path)
    [~ this]
  ::
  ++  on-arvo
    |=  [=wire gift=gift-user-v1:gall]
    ^-  (quip card _this)
    ?.  ?=([%eyre %bound *] gift)  [~ this]
    ~?  !bound.gift
      [dap.bowl "bind rejected!" binding.gift]
    [~ this]
  ::
  ++  on-leave  |=(* [~ this])
  ++  on-agent  |=(* [~ this])
  ::
  ++  on-fail
    |=  [frag:agent:gall =call:agent:gall]
    %.  [~ this]
    (slog (rap 3 dap.bowl ': on-fail: ' from ~) tang)
  ::
  ++  on-peek   |=(path `(unit (unit cage))`~)
--
|%
::  helper core
::
++  build
  |_  [=bowl:gall]
  ::
  +$  mod-bond
    $:  ped=(list perm:gall)  ::  required
        peg=(list perm:gall)  ::  granted
        peq=(list perm:gall)  ::  requested not granted
        pew=(list perm:gall)  ::  awaiting not granted
        ned=(list perm:gall)  ::  required not granted
    ==
  ::
  +$  desk-data
    $:  =desk
        mod-bond
        =zest:clay
        from=ship
        wic=(set weft)
        ese=?
        inst=?           :: installed
    ==
  ::
  ++  make-path
    |=  [=desk =spur]
    (weld /(scot %p our.bowl)/[desk]/(scot %da now.bowl) spur)
  ::
  ++  get-desk-data
    |=  [con=cone:clay target=desk]
    ^-  desk-data
    =/  sor  :: TODO: review
      .^((map desk [ship desk]) %gx (make-path %hood /kiln/sources/noun))
    ::  skip desks without sys.kelvin (happenes while installing)
    =/  from=[=ship =desk]
      ?:  (~(has by sor) target)
        (~(got by sor) target)
      [our.bowl target]
    =/  dom=dome:clay  (~(got by con) [our.bowl target])
    ::
    ?.  .^(? %cu (make-path target /sys/kelvin))
      [target *mod-bond %dead ship.from ~ | |]
    ::
    =+  .^(=bond %cx (make-path %$ /bond/[target]))
    =+  .^(ese=? %cx (make-path %$ /esse/[target]))
    [target (modify-bond bond) liv.dom ship.from ~(key by wic.dom) ese &]
  ::
  ++  modify-bond
  |=  =bond
  ^-  mod-bond
  =/  peq  ::  requested not granted
    (skip ~(tap in peq.bond) (cury have:guard:gall peg.bond))
  =/  pew  ::  awaiting not granted
    (skip ~(tap in pew.bond) (cury have:guard:gall peg.bond))
  =/  ned  ::  required not granted
    (skip ~(tap in ped.bond) (cury have:guard:gall peg.bond))
  :*  ~(tap in ped.bond)
      ~(tap in peg.bond)
      peq
      pew
      ned
  ==
  ::
  ++  page
    =+  .^(=cone:clay %cx (make-path %$ /domes))
    =/  desks=(set desk)
      (~(del in .^((set desk) %cd (make-path %$ /))) %kids)
    =/  des=(list desk-data)
      %-  turn  :_  (cury get-desk-data cone)
      ~(tap in (~(del in desks) %base))
    =/  base=desk-data  (get-desk-data cone %base)
    ^-  manx
    ;html
      ;head
        ;meta(charset "utf-8");
        ;title:"permission manager"
        ;style: {style}
      ==
      ;body
        ;div.flex.grow
          ;+  (menu base des)
          ;div.display
            ;div(class "flex-sb flex-col gap display-def")
              ;h2:"System Permissions"
              ;p:"Every app asks permission before it reads your files, talks to other agents, or sends traffic over the network. Pick an app on the left to review the decisions you've made or change your mind."
            ==
            ;*  (turn des display)
            ;+  (display-base base des)
          ==
        ==
        ;script: {script}
      ==
    ==
  ::
  ++  menu
    |=  [base=desk-data des=(list desk-data)]
    =/  [ok=(list desk-data) blocked=(list desk-data)]
      %+  skid  des
      |=  [=desk mod-bond *]
      ?&(=(~ pew) =(~ ned))
    =/  base=manx
      ;div
        ;a(href "#base", class "menu-item")
          ;h3:"%base"
        ==
      ==
    ::
    ;div.menu
      ;input(type "checkbox", class "prompt-toggle", id "/prompt", form "prompt-form");
      ;div.prompts
        ;+  (prompt blocked)
      ==
      ;form(method "post", action "install", class "menu-item gap-sm")
        ::  TODO: parse input value prior submitting
        ;input(type "text", placeholder "e.g. ~paldev/pals", name "install-desk");
        ;button(type "submit", class "btn-install"):">"
      ==
      ;*  ?~  blocked  [base ~]
          :*
            ;div.pop-up.flex.flex-col
              ;div
                ;h2:"{(scow %ud (lent blocked))} requests"
                ;p:"Apps are waiting for a decision"
              ==
              ;div.btn-row
                ;label(for "/prompt", class "btn-sm"):"Review and respond"
              ==
            ==
            base
            ;p.tiny.menu-item:"Needs Attention"
            %+  turn
              ::  sort to put blocking > requested > more perms > less perms
              ::
              %+  sort  blocked
              |=  [a=[=desk mod-bond *] b=[=desk mod-bond *]]
              =/  ned-a  (silt (welp ned.a pew.a))
              =/  ned-b  (silt (welp ned.b pew.b))
              ?:  ?=(^ ned-a)  &
              ?:  ?=(^ ned-b)  |
              ?:  ?=(^ peq.a)  &
              ?:  ?=(^ peq.b)  |
              (gth (lent peg.a) (lent peg.b))
            |=  [=desk mod-bond =zest:clay *]
            =/  color
              ?:(?=(%live zest) "#d29922" "#e0392b")
            ^-  manx
            ;div
              ;a(href "#{(trip desk)}", class "menu-item")
                ;h3:"{<desk>}"
                ;icon
                  ;span(style "background:{color};", class "icon-desk-status");
                ==
              ==
            ==
          ==
      ;*  ?~  ok  [;div; ~]
          :*
            ;p.tiny.menu-item:"Installed Apps"
            %+  turn
              %+  sort  ok
              |=  [a=[=desk mod-bond *] b=[=desk mod-bond *]]
              ?:  ?=(^ peq.a)  &
              ?:  ?=(^ peq.b)  |
              (gth (lent peg.a) (lent peg.b))
            |=  desk-data
            ^-  manx
            ;div
              ;a(href "#{(trip desk)}", class "menu-item")
                ;h3:"{<desk>}"
                ;+  ?:  =(| inst)
                      ;icon(id "spinner/{(trip desk)}")
                        ;span.spinner;
                      ==
                    ?:  =(~ peq)  ;div;
                    ;icon
                      ;span(class "icon-badge"):"{(scow %ud (lent peq))}"
                    ==
              ==
            ==
      ==
    ==
  ::
  ++  prompt
    ::  TODO: show if desk blocking %base update
    ::  if essential and base has an update
    ::  perhaps if user doesn't want to grant perms we should allow suspension?
    |=  des=(list desk-data)
    =-
      ;form(method "post", action "submit-prompt", id "prompt-form")
        ;*  -
      ==
    ^-  marl
    =/  total  (lent des)
    =|  [marl-prompt=marl i=@ud]
    |-  ?~  des  marl-prompt
    =/  [=desk mod-bond =zest:clay from=ship wic=(set weft) *]  i.des
    =/  status=tape
      ?-  zest  ::  TODO: proper check look at weft etc.
        %dead  "continue installing"
        %live  "proceed with update"
        %held  "be set live"
      ==
    =-
      %=  $
        marl-prompt  (snoc marl-prompt -)
        i            +(i)
        des          t.des
      ==
    ^-  manx
    ;div(class "prompt-wrap")
      ;input(type "checkbox", class "prompt-desk-toggle", id "/prompt/{(trip desk)}");
      ;div(class "prompt")
        ;div(class "prompt-top flex-sb")
          ;label:"{(scow %ud +(i))} of {(scow %ud total)}"
          ;button(type "reset", class "btn-wrapper")
            ;p.tiny.close:"close"
          ==
        ==
        ;div
          ;h3:"{<desk>} requires permissions"
          ;p.tiny:"can't {status} until you allow."
        ==
        ;*  ^-  marl
          =/  ned-all  (silt (welp ned pew))
          :~
              ;div(class "perm-card")
                ;*  ^-  marl
                %+  turn  ~(tap in ned-all)
                (curr perm-text `"Required")
              ==
              (perm-to-input desk ned-all)
          ==
        ;input(type "radio", name "decision-{(trip desk)}", value "allow", id "allow-{(trip desk)}", class "decision hidden");
        ;input(type "radio", name "decision-{(trip desk)}", value "deny", id "deny-{(trip desk)}", class "decision hidden");
        ;div(class "flex-sb")
          ;*  ?~  t.des
                :~  ;button(type "submit", name "decision-{(trip desk)}", value "allow", class "btn"):"Allow always"
                    ;button(type "submit", name "decision-{(trip desk)}", value "deny", class "btn"):"Deny all"
                ==
              :~  ;label(for "allow-{(trip desk)}", class "btn"):"Allow always"
                  ;label(for "deny-{(trip desk)}", class "btn"):"Deny all"
              ==
        ==
      ==
    ==
  ::
  ++  display
    |=  desk-data
    ::
    =/  peg-set=(set perm:gall)  (silt peg)
    =/  gar=(set perm:gall)   :: granted required
      =/  covered=(list (list perm:gall))
        (turn ped (cury over:guard:gall peg-set))
      (~(gas in *(set perm:gall)) (zing covered))
    =/  gor=(list perm:gall)  ::  granted optional
      ~(tap in (~(dif in peg-set) gar))
    ::
    =/  status
      ?:  ?=(%live zest)
        ?~  pew  "live"
        "Pending update, missing permissions"
      ?:  ?=(%held zest)
        ?~  pew  "suspended, awaiting update"
        "Suspended, missing permissions"
      ?~  ned  "suspended"
      "Can't set live, missing permissions"
    ::
  ::
    ?:  =(| inst)  ;div(id "installing/{(trip desk)}");
    ;div(class "display-panel gap", id "{(trip desk)}")
      ;div.display-item
        ;+  (render-desk desk ese from &)
        ;p:"{status}"
        ;+  ?:  =(~ wic)  ;div;
            ;div.flex
              ;p:"Update avaliable:"
              ;*  %+  turn  ~(tap in wic)
                  |=  =weft
                  ;p:"{<lal.weft>} {<num.weft>}"
            ==
      ==
      ;div.flex.flex-col.gap
        ;+  ?:  &(=(~ ned) =(~ pew) =(~ peq))  ;div;
            ;div(class "btn-row display-item")
              ;label(for "/prompt-all/{(trip desk)}", class "btn-sm"):"Allow all"
            ==
        ;*  =-  (murn - same)
            ^-  (list (unit manx))
            :~  ?~  ned  ~  %-  some
                (render-perms desk "Blocking install/live" & | ned ~)
              ::
                ?^  ned  ~
                ?~  pew  ~  %-  some
                (render-perms desk "Blocking upgrade" & | pew ~)
              ::
                ?~  peq  ~  %-  some
                (render-perms desk "Requested permissions" | | peq ~)
              ::
                ?:  &(=(~ gor) =(~ gar))  ~  %-  some
                (render-perms desk "Granted permissions" | & gor ~(tap in gar))
            ==
        ;+  ?:  &(=(~ ned) =(~ pew) =(~ peq))  ;div;
            =/  ned  ~(tap in (silt (weld ned pew)))  ::  de-dup perms print
            (prompt-grant-all desk ned peq)
      ==
    ==
  ::
  ++  display-base
    |=  [base=desk-data desks=(list desk-data)]
    =/  wic-l=(list weft)
      (sort ~(tap in wic.base) |=([[@tas a=@ud] [@tas b=@ud]] (gth a b)))
    =+  .^(=waft:clay %cx (make-path %base /sys/kelvin))
    =+  .^(=vere %$ (make-path %$ /zen/ver))
    ?>  ?=([%zuse @] waft)
    ::
    ^-  manx
    ;div(class "display-base gap", id "base")
      ;div(class "display-elem gap")
        ;h2:"About system:"
        ;p:"Runtime version: {(trip (slav %ta (rear rev.vere)))}"
        ;form(action "/~/logout", method "post", class "flex-sb margin-l")
          ;button(type "submit", class "btn-sm"):"Log out"
        ==
      ==
      ;div(class "display-elem gap", style "flex: 1;")
        ;div.display-item
          ;div.flex.flex-sb
            ;h2:"%base"
            ;p:"{<lal.waft>} {<num.waft>}"
          ==
          ;p.tiny:"{<from.base>}"
        ==
        ;div.display-item
          ;*  ?~  wic-l  [;p:"Up to date" ~]
              %+  turn  wic-l
              |=   =weft
              =/  blocked
                %+  skim  desks
                |=  =desk-data
                ?&  =(& ese.desk-data)
                    ?|  !(~(has in wic.desk-data) weft)
                        !=(~ pew.desk-data)
                    ==
                ==
              ::
              ?~  blocked
                =/  msg=tape
                  ?:  .^(? %$ (make-path %$ /zen/lag))
                    "Runtime doesn't support avaliable [{<lal.weft>} {<num.weft>}] update."
                  "Missing blocked desks for [{<lal.weft>} {<num.weft>}]"
                ;div
                  ;p:"{msg}"
                ==
              ;details.flex.flex-col
                ;summary:"Update avaliable: {<lal.weft>} {<num.weft>}"
                ;div.flex.flex-col.gap
                  ;+  (prompt-suspend-all blocked num.weft)
                  ;div.flex.flex-sb
                    ;p:"Update blocked on:"
                    ;label(for "/prompt-suspend/{<num.weft>}", class "btn-sm"):"Suspend all"
                  ==
                  ;*  (turn blocked (curr render-desks weft))
                ==
              ==
        ==
      ==
    ==
  ::
  ++  prompt-grant-all
    |=  [=desk ned=(list perm:gall) peq=(list perm:gall)]
    ^-  manx
    ;div
      ;input(type "checkbox", class "prompt-all-toggle", id "/prompt-all/{(trip desk)}");
      ;div(class "prompt prompt-all")
        ;div(class "flex-sb margin-l")
          ;label(for "/prompt-all/{(trip desk)}", class "close"):"close"
        ==
        ;div
          ;h2:"Allow all required and requested permissions"
          ;p:"for {<desk>} desk"
        ==
        ;div(class "perm-card")
          ;*  =-  (murn - same)
              ^-  (list (unit manx))
              :~  ?~  ned  ~  %-  some
                  ;div(class "perm-card")
                    ;*  ^-(marl (turn ned (curr perm-text `"Required")))
                  ==
                  ?~  peq  ~  %-  some
                  ;div(class "perm-card")
                    ;*  ^-(marl (turn peq (curr perm-text `"Requested")))
                  ==
              ==
        ==
        ;form(method "post", action "submit-prompt/{(trip desk)}", class "flex-sb margin-l")
          ;+  (perm-to-input desk (silt (welp ned peq)))
          ;div.flex.flex-sb.margin-l
            ;button(type "submit", name "decision-{(trip desk)}", value "allow", class "btn"):"Allow all"
          ==
        ==
      ==
    ==
  ::
  ++  prompt-suspend-all
    |=  [blocked=(list desk-data) num=@ud]
    ;div
      ;input(type "checkbox", class "prompt-suspend-toggle hidden", id "/prompt-suspend/{<num>}");
      ;form(method "post", action "suspend", class "prompt prompt-suspend")
        ;div(class "prompt-top flex-sb margin-l")
          ;label(for "/prompt-suspend/{<num>}", class "close"):"close"
        ==
        ;div
          ;h3:"Suspend desks to proceed with system update"
          ;p.tiny:"Essential desks will become non-essential, and will be suspended until it receives an update and required permissions"
        ==
        ;*  %+  turn  blocked
            |=  desk-data
            ;div.perm-data
              ;input(type "hidden", name "suspend-{(trip desk)}", value "allow");
              ;+  (render-desk desk ese from |)
            ==
        ;div.flex.flex-sb.margin-l
          ;button(type "submit", name "suspend-all", value "allow", class "btn"): Suspend
        ==
      ==
    ==
  ::
  ++  render-desks
    |=  [desk-data =weft]
    ^-  manx
    ;div.flex.flex-col.gap-sm.desk-data
      ;+  (render-desk desk ese from |)
      ;+  ?.  ?&  (~(has in wic) weft)
                  !=(~ pew)
              ==
            ;form(method "post", action "suspend", class "flex flex-sb")
              ;p.tiny:"Awaiting compatible update with: {<lal.weft>} {<num.weft>}"
              ::TODO: pop-up with explaning what it's going to do and confirm btn
              ;button(type "submit", name "suspend", value "{(trip desk)}", class "btn-sm"):"Set to non essential"
            ==
          ;form(method "post", action "submit-prompt/base", class "flex flex-col gap")
            ;+  (perm-to-input desk (silt pew))
            ;*  (turn pew (curr perm-text `"Reqired"))
            ;div.flex.flex-sb.margin-l
              ;button(type "submit", name "decision-{(trip desk)}", value "allow", class "btn-sm"):"Allow all"
            ==
          ==
    ==
  ::
  ++  render-desk
    |=  [=desk ese=? from=ship main-view=?]
    ;div.flex.flex-col
      ;div.flex.flex-sb.gap.align-c
        ;+  ?:  main-view  ;h3:"{<desk>}"
        ;p:"{<desk>}"
        ;+  ?.  ese  ;div;
            ;div.ese
              ;p:"essential"
            ==
      ==
      ;p.tiny:"{<from>}"
    ==
  ::
  ++  render-perms
    |=  [=desk nom=tape open=? granted=? pes=(list perm:gall) fixed=(list perm:gall)]
    ^-  manx
    =/  clas  ?:(open "warning" "display-item")
    ;form(method "post", action "action/{(trip desk)}", class "{clas}")
      ;+  %-  (attack 'open' open)
      ;details.flex.flex-col.gap
        ;summary:"{nom}"
        ;*  (turn pes (cury (cury (cury render-perm desk) granted) |))
        ::TODO  not disabled if not live
        ;*  (turn fixed (cury (cury (cury render-perm desk) granted) &))
        ;div(class "btn-row")
          ;button(type "submit", value "{(trip desk)}", class "btn-sm"):"save"
        ==
      ==
    ==
  ::
  ++  render-perm
    |=  [=desk granted=? disabled=? =perm:gall]
    ^-  manx
    =/  nom=tape  "{(trip desk)}/{(scow %uw (jam perm))}"  ::REVIEW
    ;label(for nom, class "flex gap")
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
      ;+  (perm-text perm ~)
      ;br;
    ==
  ::
  ++  perm-text
    |=  [=perm:gall prompt=(unit tape)]
    ^-  manx
    =;  txt=tape
      ?~  prompt  ;h3:"{txt}"
      ;div(class "perm-data flex-sb")
        ;h3:"{txt}"
        ;+
          =/  note  ?:(=(u.prompt "Requested") "request" "warning")
          ;div(class "{note}")
            ;p:"{u.prompt}"
          ==
      ==
    ::  REVIEW
    ?-  -.perm
      %super  "act with your ship's full authority, internally and on the network"
      %write  ?~  dude.perm  "poke any local agent"
              "poke %{(trip u.dude.perm)} agent"
      %watch  ?~  dude.perm  "subscribe to any local agent for {(spud path.perm)}"
              "subscribe to %{(trip u.dude.perm)} agent for {(spud path.perm)}"
      %reads  "read {(cash-text +>.perm)} from {(trip vane.perm)}"
      %fling  "%fling thing"  :: TODO
    ::
        %ames
      ?-  +<.perm
        %block  "add and remove ships from blacklist/whitelist"
        %reads  "remote scry local data {(dash-text +>.perm)}"
        %keens  "remote scry data from other ships"
        %write  "modify agent namespace"
        %debug  "modify debug settings"
      ==
    ::
        %behn
      ?-  +<.perm
        %timer  "set timers"
      ==
    ::
        %clay
      ?-  +<.perm
        %write  "write files {(dash-text +>.perm)}"
        %local  "read files on this ship {(cash-text +>.perm)}"
        %peers  "read files from other ships"
        %crews  "manage filesystem permissions groups"  ::??
        %rules  ?~  des.perm  "manage filesystem permissions on any desk"
                "manage filesystem permissions on %{(trip u.des.perm)} desk"
        %stone  ?~  des.perm  "permanently delete files and desks versions on any desk"
                "permanently delete files and desks versions on %{(trip u.des.perm)} desk"
        %mount  "mount desk to the host filesystem"
        %desks  ?~  des.perm  "install, suspend, revive, or mark any desk as essential"
                "suspend, revive, or mark %{(trip u.des.perm)} desk as essential"
        %pulse  "subscribe to desk liveness and pending updates"
        %perms  "change userspace permissions"
        %guard  "subscribe to userspace permissions updates"
      ==
    ::
        %dill
      ?-  +<.perm
        %terms  "manage your terminal sessions"
        %print  "print to termianal"
        %sylog  "subscribe to system output"
        %weigh  "run a memory usage report"
        %press  "optimise ship memory usage"
      ==
    ::
        %eyre
      ?-  +<.perm
        %setup  "manage web server and eauth configurations"
        %serve  "serve web content at {(spud path.perm)}"
        %cross  "manage which sites can make requests to your ship (CORS)"
        %debug  "modify debug print settings for eyre"
      ==
    ::
        %gall
      ?-  +<.perm
        %clear  ?~  dude.perm  "delete any agent, permanently erase its data"
                "delete %{(trip u.dude.perm)} agent, permanently erase its data"
        %debug  "modify debug print settings for gall"
      ==
    ::
        %iris
      ?-  +<.perm
        %fetch  "make http requests"
      ==
    ::
        %jael
      ?-  +<.perm
        %privy  "subscribe to private-key updates"
        %watch  "manage public-keys tracking source for ships"
        %moons  "create and manage moons"
        %rekey  "change your private-keys"
        %login  "reset web login code"
        %blast  "simulate breach, will break communication with given ships"
      ==
    ::
        %khan
      ?-  +<.perm
        %twine  "start threads"
      ==
    ::
        %lick
      ?-  +<.perm
        %ports  "manage IPC ports"
      ==
    ==
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
    =/  spur-txt
      ?~  spur  "any path"
      (spud spur)
    ?~  desk  "in any desk at {spur-txt}"
    "in %{(trip u.desk)} desk at {spur-txt}"
  ::
  ++  perm-to-input
    |=  [=desk pez=(set perm:gall)]
    ^-  manx
    =/  val=tape  (scow %uw (jam pez))
    ;input(type "hidden", name "perms-{(trip desk)}", value val);
  ::  maybe attach attribute
  ::
  ++  attack
    |=  [at=$@(@t [nom=@t val=@t]) do=?]
    ?.  do  same
    |=  manx
    [[n.g [?@(at [at ""] [nom.at (trip val.at)]) a.g]] c]
  ::
  ::
  ++  script
    ^~
    %-  trip
    '''
    var STATUS_COLOR = {
      live: "white",    //  live, nothing
      held: "#d29922",  //  live, update blocked
      dead: "#e0392b"   //  not-live, blocked
    };

    function boot() {
      var seen = new Set();
      document.querySelectorAll("[id^='spinner/']").forEach(function (el) {
        var parts = el.id.split("/");                     // ["spinner", desk]
        var desk = parts[1];
        if (!seen.has(el.id)) { seen.add(el.id); poll(desk); }
      });
    }
    function poll(desk) {
      var t = setInterval(function () {
        fetch("desk/" + encodeURIComponent(desk)).then(function (r) {
          if (r.status === 204) return;           // not ready → keep polling
          var status = r.headers.get("install-status");
          r.text().then(function (html) {
            clearInterval(t);
            apply(desk, html, status);
          });
        });
      }, 20000);
    }
    function apply(desk, html, status) {
      var elIcon = document.getElementById("spinner/" + desk);
      if (elIcon) {
        var span = elIcon.querySelector("span.spinner");
        if (span) {
          span.classList.remove("spinner");
          span.classList.add("icon-desk-status");
          span.style.background = STATUS_COLOR[status] || STATUS_COLOR.dead;
        }
      }

      var el = document.getElementById("installing/" + desk);
      if (!el) return;
      var doc = new DOMParser().parseFromString(html, "text/html");
      el.replaceWith(...doc.body.childNodes);
    }

    //  initInstall(): submit install request
    //
    //    parses input, only acts when valid
    //
    function initInstall() {
      var form = document.querySelector('form[action="install"]');
      if (!form) return;
      var input = form.querySelector('input[name="install-desk"]');
      if (!input) return;

      var msg = document.createElement("p");
      msg.className = "tiny";
      msg.style.color = "#e0392b";
      msg.style.margin = "4px 0 0";
      msg.style.display = "none";
      form.parentNode.insertBefore(msg, form.nextSibling);

      function clearError() {
        input.style.borderColor = "";
        msg.style.display = "none";
      }
      function showError(text) {
        input.style.borderColor = "#e0392b";
        msg.textContent = text;
        msg.style.display = "block";
      }
      input.addEventListener("input", clearError);

      form.addEventListener("submit", function (e) {
        e.preventDefault();
        clearError();
        if (!input.value.trim()) { showError("Enter a publisher ship and desk to install"); return; }
        fetch("install", {
          method: "POST",
          headers: { "content-type": "application/x-www-form-urlencoded" },
          body: input.value,
          redirect: "manual"
        }).then(function (r) {
          if (r.type === "opaqueredirect" || r.ok) window.location.reload();
          else showError("Not valid syntax, try e.g. ~paldev/pals");
        }).catch(function () {
          showError("Couldn't reach the ship, try again");
        });
      });
    }

    boot();
    initInstall();
    '''
  --
::
++  style
  ^~
  %-  trip
  '''
  :root {
    --font-grey: #666666;
    --grey-1:    #1A181814;
    --grey-2:    #E5E5E5;
    --grey-3:    #F5F5F5;
    --blue-1:    #3B80E8;
    --blue-2:    #CCDCF3;
    --blue-3:    #F5FAFF;
    --red-1:     #E22A2A;
    --red-2:     #FEF5F5;
  }
  body {
    font-family: sans-serif;
    font-weight: 400;
    margin: 0;
    height: 100vh;
    overflow: hidden;
    display: flex;
    flex-direction: column;
    }
  h2 {
    font-size: 17px;
    font-weight: 400;
    display: inline-block;
    }
  h3 {
    font-size: 17px;
    font-weight: 350;
    margin: 0;
    }
  p {
    font-size: 16px;
    font-weight: 300;
    }
  p.tiny, label {
    font-size: 14px;
    font-weight: 270;
    color: var(--font-grey);
    }
  a {
    color: inherit;
    text-decoration: none;
    }
  summary {
    cursor: pointer;
  }
  div {
    padding: 0;
    }
  div > * {
    margin: 0;
    }
  input[type="text"] {
    font: inherit;
    width: 100%;
    height: 30px;
    padding: 4px;
    border: 1px solid #000;
    border-radius: 6px;
  }
  details:not([open]) {
    gap: 0;
  }
  /*
    Universal styling classes
  */
  .flex {
    display: flex;
    }
  .flex-sb {
    display: flex;
    justify-content: space-between;
    align-items: center;
    }
  .flex-col {
    flex-direction: column;
  }
  .align-c{
    align-items: center;
  }
  .gap {
    gap: 24px;
  }
  .gap-sm {
    gap: 6px;
  }
  .grow {
    flex: 1;
    min-height: 0;
    }
  .margin-l {
    margin-left: auto;
  }
  .hidden {
    display: none;
  }
  /*
    Elements styling
  */
  .btn, .btn-sm, .btn-install {
    box-sizing: border-box;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    white-space: nowrap;
    border: 1px solid #000;
    background: #fff;
    color: #000;
    font: inherit;
    cursor: pointer;
  }
  .btn {
    width: 209px;
    height: 60px;
    border-radius: 16px;
    padding: 12px 24px;
    }
  .btn-sm {
    height: 30px;
    min-width: 108px;
    border-radius: 10px;
    padding: 4px 21px;
    }
  .btn-install{
    height: 30px;
    width: 30px;
    border-radius: 6px;
    padding: 0;
  }
  .btn:hover, .btn-sm:hover, .btn-install:hover {
    background: #000;
    color: #fff;
    }
  .icon-desk-status {
    display:inline-block;
    width:.6em;
    height:.6em;
    border-radius: 50%;
    }
  .spinner {
    display: inline-block;
    width: .9em;
    height: .9em;
    border: 2px solid var(--grey-2);
    border-top-color: var(--font-grey);
    border-radius: 50%;
    animation: spin .7s linear infinite;
    }
  @keyframes spin {
    to { transform: rotate(360deg); }
    }
  .icon-badge {
    box-sizing: border-box;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    min-width: 1.6em;
    height: 1.6em;
    padding: 0 .35em;
    border-radius: 50%;
    background: var(--grey-1);
    font-size: .85em;
    line-height: 1;
    }
  .perm-card {
    display: flex;
    flex-direction: column;
    gap: 6px;
    flex: 1 1 auto;
    overflow-y: auto;
    }
  .perm-data {
    border: 1px solid var(--grey-2);
    border-radius: 16px;
    padding: 12px;
    }
  .desk-data {
    border: 1px solid #000;
    border-radius: 8px;
    padding: 12px;
  }
  /*
    Wrapper element styling
  */
  .btn-row {
    display: flex;
    gap: 12px;
    justify-content: flex-end;
    }
  .btn-row:has(:nth-child(2)) {
    justify-content: space-between;
    }
  .btn-wrapper {
    padding: 0;
    border: none;
    font: inherit;
    background: none;
  }
  .btn-wrapper > * {
    margin: 0;
    }
  .prompt {
    box-sizing: border-box;
    position: fixed;
    top: 50%;
    left: 50%;
    transform: translate(-50%,-50%);
    display: flex;
    flex-direction: column;
    gap: 24px;
    padding: 24px;
    width: 500px;
    max-height: calc(100vh - 48px);
    background: #fff;
    border: 1px solid var(--grey-2);
    border-radius: 16px;
    box-shadow: 0 2px 16px rgba(0,0,0,.25);
    }
  .prompt .close {
    display: inline-block;
    cursor: pointer;
    color: var(--font-grey);
    }
  .prompt-top {
    padding: 0;
    }
  .prompt-top > * {
    margin: 0;
    }
  .menu {
    flex: 1;
    display: flex;
    flex-direction: column;
    gap: 16px;
    border-right: 1px solid var(--grey-2);
    padding: 12px 6px 12px 12px;
    min-height: 0;
    overflow-y: auto;
    scrollbar-width: none;
    }
  .menu::-webkit-scrollbar {
    display: none;
    }
  .menu-item {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 6px 0;
    }
  .display {
    flex: 3;
    display: flex;
    flex-direction: column;
    padding: 24px;
    background: var(--grey-3);
    min-height: 0;
    overflow-y: auto;
    scrollbar-width: none;
    }
  .display::-webkit-scrollbar {
    display: none;
    }
  .display-item, .warning, .request {
    padding: 6px 8px;
  }
  .display-elem {
    display: flex;
    flex-direction: column;
    width: 100%;
    box-sizing: border-box;
    background: #fff;
    border: 1px solid var(--grey-2);
    border-radius: 12px;
    padding: 12px;
  }
  .pop-up{
    color: var(--blue-1);
    background: var(--blue-3);
    border: 1px solid var(--blue-2);
    border-radius: 6px;
    padding: 24px;
    gap: 24px;
    }
  .pop-up > .btn-row > .btn-sm {
    background: var(--blue-1);
    border: 1px solid var(--blue-1);
    color: var(--blue-3);
    padding: 5px 20px;
    }
  .ese{
    background: var(--blue-1);
    color: var(--blue-3);
    border: 1px solid var(--blue-1);
    border-radius: 10px;
    padding: 5px 5px;
  }
  .warning{
    background: var(--red-2);
    color: var(--red-1);
    border: 1px solid var(--red-2);
    border-radius: 12px;
  }
  .request{
    background: var(--grey-1);
    color: var(--font-grey);
    border: 1px solid var(--grey-1);
    border-radius: 12px;
  }
  /*
    Display toggles
  */
  .display-def {
    margin: auto;
    max-width: 500px;
    }
  .display-panel {
    display: none;
    }
  .display-panel:target {
    display: flex;
    flex-direction: column;
    width: 100%;
    flex: 1;
    box-sizing: border-box;
    background: #fff;
    border: 1px solid var(--grey-2);
    border-radius: 12px;
    padding: 12px;
    }
  .display-base {
    display: none;
    }
  .display-base:target {
    display: flex;
    flex-direction: column;
    width: 100%;
    flex: 1;
    box-sizing: border-box;
  }
  .display:has(.display-panel:target) .display-def,
  .display:has(.display-base:target) .display-def {
    display: none;
    }
  .prompt-toggle {
    display: none;
    }
  .prompts {
    display: none;
    background: none;
    z-index: 0;
    }
  .prompt-toggle:checked ~ .prompts {
    display: block;
    position: fixed;
    inset: 0;
    background: rgba(0,0,0,.4);
    z-index: 100;
    }
  .prompt-desk-toggle {
    display: none;
    }
  .prompt-desk-toggle:checked + .prompt {
    display: none;
    }
  .prompt-wrap:has(.decision:checked) > .prompt {
    display: none;
    }
  .prompt-wrap:not(:has(.decision:checked)) ~ .prompt-wrap > .prompt {
    display: none;
    }
  .prompt-all {
    display: none;
    }
  .prompt-all-toggle {
    display: none;
    }
  .prompt-all-toggle:checked ~ .prompt-all {
    display: flex;
    }
  .prompt-suspend {
    display: none;
    }
  .prompt-suspend-toggle:checked ~ .prompt-suspend {
    display: flex;
    }
  /*  */
  '''
--
