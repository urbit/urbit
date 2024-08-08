/@  wizard
/@  wizard-poke
/-  serv=sky-server
/-  srv=server
/-  manx-utils
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
    =/  =binding:eyre  [~ ~[%wizard]]
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
      ?.  authenticated.req
        %+  ~(respond neo:srv #/[p/our.bowl]/$/eyre)
          eyre-id
        (login-redirect:gen:srv request.req)
      =/  purl  (parse-url:serv request.req)
      =/  the-pith  (pave:neo pax:purl)
      ?+    method.request.req  ~|(%unsupported-http-method !!)
        ::
        ::  Serve either the wizard chooser
        ::  or a specific wizard.
          %'GET'
        =/  renderer  (snag 1 the-pith)
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
                ?:  =([%n ~] renderer)
                  (chooser [~(key by state) the-pith])
                ?^  renderer  
                  ~|('Second iota in URL must be a @tas.' !!)
                (~(got by state) renderer)
            ==
        ::
            (done-card - eyre-id)
        ==
        ::
        ::  Unpack request into a made:neo, %make it,
        ::  and then redirect to tree/new-shrub.
          %'POST'
        =/  body=(map @t @t)
          (parse-form-body:serv request.req)
        ::
        ~&  >  the-pith
        =/  loc=pith
          %+  weld 
            (oust [0 2] the-pith)
          !<  pith:neo
          %+  slap  
            !>(..zuse)
          %-  ream 
          (~(got by body) 'pith')
        ~&  >  loc
        ::
        =/  stud=@tas
          !<  @tas
          %+  slap
            !>(~)
          %-  ream
          (~(got by body) 'stud')
        ::
        =/  head-pail=@tas
          !<  @tas
          %+  slap
            !>(~)
          %-  ream
          (~(got by body) 'head-pail')
        ::
        =/  vase=vase
          %+  slap  
            !>(..zuse)
          %-  ream 
          (~(got by body) 'vase')
        ::
        =/  conf=conf:neo
          =/  c  (~(get by body) 'conf')
          ?~  c  ~
          !<  conf:neo  
          %+  slap  
            !>(..zuse)
          %-  ream 
          (need c)
        ::
        %+  welp
          %+  ~(respond neo:srv #/[p/our.bowl]/$/eyre)
            eyre-id
          [[301 ['location' (en-cord:pith:neo (welp #/tree loc))]~] ~]
        :~  :*  loc
                %make
                stud
                [~ head-pail vase]
                conf
            ==
        ==
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
  |=  [options=(set stud:neo) the-pith=pith]
  ;html
    ;head
      ;meta(charset "UTF-8");
      ;title: Wizards
      ;*  standard-head-tags:serv
    ==
    ;body
      ;*
      %+  turn
        ~(tap in options)
      |=  =stud:neo
      ?^  stud  :: XX will need to change this someday
        ~|("The stud you chose isn't a @tas. Let the devs know if you ever see this error." !!)
      ^-  manx
      =/  link=tape
        ;:  welp 
          "/wizard/" 
          (trip stud) 
          (en-tape:pith:neo (oust [0 2] the-pith))
        ==
      ;a
        =href  link
        ; {(trip stud)} 
      ==
    ==
  ==
--