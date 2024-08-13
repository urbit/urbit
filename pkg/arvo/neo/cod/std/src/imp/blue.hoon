/@  eyre-reqs
/-  serv=sky-server
/-  srv=server
^-  kook:neo
|%
++  state  pro/%sig
++  poke   (sy %eyre-task ~)
++  kids
  :+  ~  %y
  %-  malt
  :~  :-  [|/%ta |]
      [pro/%eyre-task (sy %rely ~)]
  ==
++  deps  *deps:neo
++  form
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  init
    |=  pal=(unit pail:neo)
    :_  sig/!>(~)
    =/  =pith:neo  #/[p/our.bowl]/$/eyre
    :~  [pith %poke eyre-req/!>([%connect [~ ~[%blue]] here.bowl])]
        [pith %poke eyre-req/!>([%connect [~ ~[%sky]] here.bowl])]
    ==
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?+    stud  ~|(bad-stud/stud !!)
        %eyre-task
      =+  !<(=task:eyre:neo vax)
      =/  [eyre-id=@ta req=inbound-request:eyre]  task
      =/  inner=pith:neo
        (pave:neo pax:(parse-url-frfr:serv request.req))
      ::
      ::  handle the /sky route
      ?:  =(%sky (snag 0 inner))
        :_  sig/!>(~)
        :~  :*  (welp here.bowl [[%ta eyre-id] ~])
                %make
                %sky-ui
                `[%eyre-task !>(task)]
                (~(gas by *crew:neo) src/#/[p/our.bowl]/sky ~)
            ==
        ==
      ::
      ::  handle everything under /blue
      =/  renderer  (snag 1 inner)
      ?^  renderer
        ~|('Second iota in URL must be a @tas.' !!)
      :_  sig/!>(~)
      :~  :*  (welp here.bowl [[%ta eyre-id] ~])
              %make
              renderer
              `[%eyre-task !>(task)]
              (~(gas by *crew:neo) src/(slag 2 inner) ~)
          ==
      ==
    ==
  --
--
