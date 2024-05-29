/@  sky
/@  sky-diff
|%
++  state  pro/%sky
++  poke   (sy %sky %sky-diff ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~
    :-  [&/%settings |]
    [pro/%sky-settings (sy %sky-settings ~)]
  ==
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    =+  !<(this=sky q.pail)
    ?+  stud  !!
      %sky  [~ sky/vax]
      %sky-diff
        =/  poke  !<(sky-diff vax)
        ?+  -.poke  !!
          %new-tab
            :-  ~
            =.  hawks.this  [[now.bowl /home] hawks.this]
            =.  open.this  (min 4 +(open.this))
            sky/!>(this)
          %move-tab
            :-  ~
            =.  hawks.this  (snap hawks.this slot.poke [now.bowl pith.poke])
            sky/!>(this)
          %minimize
            :-  ~
            =.  hawks.this
              ;:  welp
                (scag slot.poke hawks.this)
                (slag +(slot.poke) hawks.this)
                [(snag slot.poke hawks.this) ~]
              ==
            =.  open.this  (dec open.this)
            sky/!>(this)
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
            sky/!>(this)
          %close
            :-  ~
            =.  hawks.this  (oust [slot.poke 1] hawks.this)
            =?    open.this
                (lth slot.poke open.this)
              (dec open.this)
            sky/!>(this)
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
            sky/!>(this)
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
            sky/!>(this)
          ::
        ==
      ::
    ==
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :-
      :~  [(welp here.bowl /settings) %make %sky-settings ~ ~]
          [~[p/our.bowl %home] %make %home ~ ~]
      ==
    :-  %sky
    !>
    :_  1
    :~
      [now.bowl ~[p/our.bowl %home]]
    ==
  --
--
