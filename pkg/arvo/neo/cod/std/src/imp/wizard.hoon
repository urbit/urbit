/@  wizard
/@  wizard-poke
/-  serv=sky-server
/-  srv=server
=<
^-  kook:neo
|%
++  state  pro/%wizard
++  poke   (sy %wizard-poke %eyre-task ~)
++  kids  *kids:neo
++  deps   *deps:neo
++  form
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  init
    |=  pal=(unit pail:neo)
    :_  wizard/!>(~)
    =/  =pith:neo  #/[p/our.bowl]/$/eyre
    =/  =binding:eyre  [~ ~[%neo %wizard]]
    =/  =req:eyre:neo  [%connect binding here.bowl]
    :~  [pith %poke eyre-req/!>(req)]
    ==
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    =/  state  !<(wizard q.pail)
    ?+    stud  ~|(bad-stud/stud !!)
        %wizard-poke
      =/  poke  !<(wizard-poke vax)
      ?-    -.poke
          %add-wizard
        [~ wizard/!>((~(put by state) stud.poke manx.poke))]
          %del-wizard
        [~ wizard/!>((~(del by state) stud.poke))]
      ==
    ::
        %eyre-task
      :_  pail
      =+  !<(=task:eyre:neo vax)
      =/  [eyre-id=@ta req=inbound-request:eyre]  task
      =/  request=request:http  request.req
      ?.  authenticated.req
        %+  ~(respond neo:srv #/[p/our.bowl]/$/eyre)
          eyre-id
        (login-redirect:gen:srv request)
      =/  purl  (parse-url:serv request.req)
      =/  =pith:neo  (pave:neo pax:purl)
      ?+    method.request  ~|(%unsupported-http-method !!)
        ::
        ::  Serve either the wizard chooser
        ::  or a specific wizard.
          %'GET'
        ~&  >  pith
        =/  renderer  (snag 1 pith)
        ~&  >  renderer
        ?^  renderer  ~|('Second iota in URL must be a @tas.' !!)
        =+  #/[p/our.bowl]/$/eyre
        :~  (head-card - eyre-id)
        ::
            :*  - 
                %poke 
                %eyre-sign
                !>
                :+  eyre-id 
                  %data
                :-  ~
                %-  manx-to-octs
                ?:  =(~ renderer)
                  chooser
                (~(got by state) renderer)
            ==
        ::
            (done-card - eyre-id)
        ==
        ::
        ::  Unpack into a made:neo, %make it,
        ::  and then redirect to tree/new-shrub.
          %'POST'
        ~
        ::=/  name  ::  XX ensure they encode this too
        ::=/  loc  (weld pith name)
        :::~  :*  (weld pith name)
        ::        %make
        ::        (~(got by pam.purl) 'made')  :: XX what style of encoding feels good here?
        ::    ==
        
        :::~  (head-card - eyre-id)
        ::    (redirect:srv (crip (en-tape:pith:neo loc)))
        ::    (done-card - eyre-id)
        ::==
        
        ::==
      ==
    ==
  --
--
::
|%
++  head-card
  |=  [=pith eyre-id=@ta]
  :*  pith
      %poke 
      %eyre-sign
      !>
      :^    eyre-id
          %head 
        200
      ['content-type' 'text/html']~
  ==
::
++  done-card
  |=  [=pith eyre-id=@ta]
  [pith %poke eyre-sign/!>([eyre-id %done ~])]
::
++  manx-to-octs
  |=  man=manx
  (as-octt:mimes:html (en-xml:html man))
::
++  chooser
  ;html
    ;head
      ;meta(charset "UTF-8");
      ;title: Wizards
    ==
    ;body
      ;p
        ; Placeholder  :: XX dropdown goes here
      ==
    ==
  ==
--