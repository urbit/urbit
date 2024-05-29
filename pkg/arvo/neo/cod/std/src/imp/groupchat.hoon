/@  groupchat
/@  groupchat-diff
^-  kook:neo
|%
++  state  pro/%groupchat
++  poke  (sy %groupchat-diff ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%pub |]
      [pro/%message-pub (sy %sig ~)]
      :-  [|/%sub |]
      [pro/%message-sub (sy %sig ~)]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo state=pail:neo]
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    ::  default case: make new groupchat with self as only member,
    ::  and subscribe to that publisher
    ::  XX - maybe move ordering is unpredictable here
    ?~  old
      :_  groupchat/!>([~ (snoc here.bowl %pub)])
      :~  :-  (snoc here.bowl %pub) 
          [%make %message-pub ~ ~]
          ::
          :-  (snoc here.bowl %sub) 
          [%make %message-sub ~ (malt ~[[%pub (snoc here.bowl %pub)]])]
      ==
    ::  otherwise, I've been created as an invitee to
    ::  someone else's groupchat
    ?>  =(%groupchat-diff p.u.old)
    =/  poke  !<(groupchat-diff q.u.old)
    ?+    -.poke  !!
        %invited
      :_  groupchat/!>([~ (snoc chat.poke %pub)])
      :~  :-  (snoc here.bowl %sub) 
          [%make %message-sub ~ (malt ~[[%pub (snoc chat.poke %pub)]])]
          ::
          :-  chat.poke 
          [%poke groupchat-diff/!>([%acked ~])]
      ==
    ==
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?>  =(%groupchat-diff stud)
    =/  sta  !<(groupchat q.state)
    =/  poke  !<(groupchat-diff vax)
    ?+    -.poke  !!
        :: if I'm the host, poke someone's provider to invite them to chat
        %invite
      ?>  =(our ship.src):bowl
      ::  ?>  =(our.bowl ->.publisher.sta) :: XX need @p, have @t ?
      :_  :-  %groupchat
          !>(sta(pending (~(put in pending.sta) ship.poke)))
      :~  :-  provider.poke
          [%poke groupchat-diff/!>([%invited here.bowl])]
      ==
    ::
        :: when invitee acks, remove them from pending
        :: and add them to pub's permissions
        %acked
      ?>  (~(has in pending.sta) ship.src.bowl)
      :_  :-  %groupchat
          !>(sta(pending (~(del in pending.sta) ship.src.bowl)))
      :~  :-  publisher.sta
          [%poke ships-diff/!>([%put ship.src.bowl])]
      ==
    ::
        %post
      :_  state
      :~  :-  publisher.sta
          [%poke txt/!>(text.poke)]
      ==
    ==
  --
--