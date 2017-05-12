::
::::  /hoon/talk/lib
  ::
  ::  This file is in the public domain.
  ::
/?    310
/-    talk
!:
::::
  ::
[. ^talk]
|_  bol/bowl
++  main                                                ::  main story
  |=  our/ship  ^-  cord
  =+  can=(clan our)
  ?+  can  %porch
    $czar  %court
    $king  %floor
  ==
::
++  said-url                                            ::  app url
  |=  url/purl
  :^  ost.bol  %poke  /said-url
  :+  [our.bol %talk]  %talk-action
  ^-  action
  :+  %phrase
    [[%& our.bol (main our.bol)] ~ ~]
  [%app dap.bol (crip (earn url))]~   :: XX
::
++  said                                                ::  app message
  |=  {our/@p dap/term now/@da eny/@uvJ mes/(list tank)}
  :-  %talk-action
  ^-  action
  :-  %convey
  |-  ^-  (list thought)
  ?~  mes  ~
  :_  $(mes t.mes, eny (sham eny mes))
  ^-  thought
  :+  (shaf %thot eny)
    [[[%& our (main our)] [*envelope %pending]] ~ ~]
  [now *bouquet [%app dap (crip ~(ram re i.mes))]]
::
++  uniq
  |=  eny/@uvJ
  ^-  (quid serial eny)
  [(shaf %serial eny) (shax eny)]
::
++  change-glyphs                                       ::<  ...
  ::>
  ::
  |=  {gys/(jug char (set partner)) bin/? gyf/char pas/(set partner)}
  ^+  gys
  ::  simple bind.
  ?:  bin  (~(put ju gys) gyf pas)
  ::  unbind all of glyph.
  ?~  pas  (~(del by gys) gyf)
  ::  unbind single.
  (~(del ju gys) gyf pas)
::
++  change-nicks                                        ::<  ...
  ::>
  ::
  |=  {nis/(map ship cord) who/ship nic/cord}
  ^+  nis
  ?:  =(nic '')
    (~(del by nis) who)
  (~(put by nis) who nic)
::
++  change-friends                                      ::<  ...
  ::>
  ::
  |=  {cis/(set circle) add/? cir/circle}
  ^+  cis
  %.  cir
  ?:  add
    ~(put in cis)
  ~(del in cis)
::
++  change-config                                       ::<  ...
  ::>
  ::
  |=  {cof/config dif/diff-config}
  ^+  cof
  ?-  -.dif
    $full     cof.dif
    $caption  cof(cap cap.dif)
    $filter   cof(fit fit.dif)
    $remove   cof
    ::
      $source
    %=  cof
        src
      %.  `(set partner)`pas.dif  ::TODO?  why do we *need* to cast?
      ?:  add.dif
        ~(uni in src.cof)
      ~(dif in src.cof)
    ==
    ::
      $permit
    %=  cof
        ses.con
      %.  sis.dif
      ?:  add.dif
        ~(uni in ses.con.cof)
      ~(dif in ses.con.cof)
    ==
    ::
      $secure
    %=  cof
        sec.con
      sec.dif
      ::
        ses.con
      ?.  .=  ?=(?($white $green) sec.dif)
              ?=(?($white $green) sec.con.cof)
        ~
      ses.con.cof
    ==
    ::
      $federal
    %=  cof
        fes.fed
      ?.  fed.dif  fes.fed.cof
      %.  sis.dif
      ?:  add.dif
        ~(uni in fes.fed.cof)
      ~(dif in fes.fed.cof)
      ::
        may.fed
      ?:  fed.dif  may.fed.cof
      %.  sis.dif
      ?:  add.dif
        ~(uni in may.fed.cof)
      ~(dif in may.fed.cof)
    ==
  ==
::
++  change-status                                       ::<  ...
  ::>
  ::
  |=  {sat/status dif/diff-status}
  ^+  sat
  ?-  -.dif
    $full       sat.dif
    $presence   sat(pec pec.dif)
    $remove     sat
    ::
      $human
    %=  sat
        man
      ?-  -.dif.dif
        $full     man.dif.dif
        $true     [tru.dif.dif han.man.sat]
        $handle   [tru.man.sat han.dif.dif]
      ==
    ==
  ==
--
