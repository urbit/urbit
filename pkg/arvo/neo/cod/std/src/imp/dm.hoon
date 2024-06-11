/@  dm-diff
^-  kook:neo
|%
++  state  pro/%ship  :: who I'm chatting with
++  poke  (sy %dm-diff ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%theirs |]
      [pro/%message-pub (sy %sig ~)]
      :-  [|/%mine |]
      [pro/%message-sub (sy %sig ~)]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo state=pail:neo]
  ++  init
    |=  old=(unit pail:neo)  
    ^-  (quip card:neo pail:neo)
    ?~  old  !!
    ?>  =(%dm-diff p.u.old)
    =/  poke  !<(dm-diff q.u.old)
    ?+    -.poke  !!
        :: create me with a pith to a service provider
        :: to start a new DM with them
        %initiate
      :_  ship/!>(partner.poke)
      :~  :-  (snoc here.bowl %pub)
          [%make %message-pub ~ ~]
          ::
          :-  provider.poke 
          [%poke dm-diff/!>([%invited our.bowl here.bowl])]
      ==
    ::
        :: create me with a pith to an inviter's dm
        :: to accept their DM request
        %invited
      :_  ship/!>(partner.poke)
      :~  :-  (snoc here.bowl %pub) 
          [%make %message-pub ~ ~]
          ::
          :-  (snoc here.bowl %sub) 
          [%make %message-sub ~ (malt ~[[%pub (snoc dm.poke %pub)]])]
          ::
          :-  dm.poke 
          [%poke dm-diff/!>([%acked here.bowl])]
      ==
    ==
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?>  =(%dm-diff stud)
    =/  poke  !<(dm-diff vax)
    ?+    -.poke  !!
        :: invitee pokes me with a pith to their DM
        :: to finalize the negotiation
        %acked
      =/  partner  !<(ship q.state)
      ?>  =(partner ship.src.bowl)
      :_  state
      :~  :-  (snoc here.bowl %sub) 
          [%make %message-sub ~ (malt ~[[%pub (snoc dm.poke %pub)]])]
      ==
    ::
        %post
      ?>  =(our ship.src):bowl
      :_  state
      :~  :-  (snoc here.bowl %pub)
          [%poke txt/!>(text.poke)]
      ==
    ==
  --
--