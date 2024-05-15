/@  sky
/@  sky-diff
|%
++  state  %sky
++  poke   (sy %sky %sky-diff ~)
++  kids
  %-  ~(gas by *kids:neo)
  :~
    :-  [&/%settings |]
    [%sky-settings %sky-settings]
  ==
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?+  stud  !!
      %sky  [~ vax]
      %sky-diff
        =/  poke  !<(sky-diff vax)
        =/  this  !<(sky state-vase)
        ?+  -.poke  !!
          %new-tab
            :-  ~
            =.  hawks.this  [[now.bowl /home] hawks.this]
            =.  open.this  (min 4 +(open.this))
            !>(this)
          %move-tab
            :-  ~
            =.  hawks.this  (snap hawks.this slot.poke [now.bowl pith.poke])
            !>(this)
          %minimize
            :-  ~
            =.  hawks.this
              ;:  welp
                (scag slot.poke hawks.this)
                (slag +(slot.poke) hawks.this)
                [(snag slot.poke hawks.this) ~]
              ==
            =.  open.this  (dec open.this)
            !>(this)
          %maximize
            :-  ~
            =.  hawks.this
              ;:  welp
                [(snag slot.poke hawks.this) ~]
                (scag slot.poke hawks.this)
                (slag +(slot.poke) hawks.this)
              ==
            =?    open.this
                (gte slot.poke open.this)
              (min 4 +(open.this))
            !>(this)
          %close
            :-  ~
            =.  hawks.this  (oust [slot.poke 1] hawks.this)
            =?    open.this
                (lth slot.poke open.this)
              (dec open.this)
            !>(this)
          %slide-up
            :-  ~
            =?  hawks.this
              (gth slot.poke 0)
              ;:  welp
                (scag (dec slot.poke) hawks.this)
                [(snag slot.poke hawks.this) ~]
                [(snag (dec slot.poke) hawks.this) ~]
                (slag +(slot.poke) hawks.this)
              ==
            !>(this)
          %slide-down
            :-  ~
            =?  hawks.this
              (lth slot.poke 3)
              ;:  welp
                (scag slot.poke hawks.this)
                [(snag +(slot.poke) hawks.this) ~]
                [(snag slot.poke hawks.this) ~]
                (slag (add 2 slot.poke) hawks.this)
              ==
            !>(this)
          ::
        ==
      ::
    ==
  ++  init
    |=  vas=(unit vase)
    ^-  (quip card:neo vase)
    :-
      :~  [(welp here.bowl /settings) %make %sky-settings ~ ~]
          [~[p/our.bowl %home] %make %home ~ ~]
      ==
    !>
    :_  1
    :~
      [now.bowl ~[%home]]
    ==
  --
--
