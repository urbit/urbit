::
::
/-  hall
/+  chat, hall-json
::
|_  act=action:chat
++  grow
  |%
  ++  tank  !!
  --
::
++  grab
  |%
  ++  noun  action:chat
  ++  json
    |=  jon=^json
    =<  (parse-chat-action jon)
    |%
    ::
    ++  hall-action
      =,  dejs:hall-json
      =,  dejs-soft:format
      |=  a=^json
      ^-  action:hall
      =-  (need ((of -) a))
      :~  create+(ot nom+so des+so sec+secu ~)
          design+(ot nom+so cof+conf ~)
          delete+(ot nom+so why+(mu so) ~)
          depict+(ot nom+so des+so ~)
          filter+(ot nom+so fit+filt ~)
          permit+(ot nom+so inv+bo sis+(as (su fed:ag)) ~)
          source+(ot nom+so sub+bo srs+(as sorc) ~)
          read+(ot nom+so red+ni ~)
          usage+(ot nom+so add+bo tas+(as so) ~)
          newdm+(ot sis+(as (su fed:ag)) ~)
          ::
          convey+(ar thot)
          phrase+(ot aud+audi ses+(ar spec:dejs:hall-json) ~)
          ::
          notify+(ot aud+audi pes+(mu pres) ~)
          naming+(ot aud+audi man+huma ~)
          ::
          glyph+(ot gyf+so aud+audi bin+bo ~)
          nick+(ot who+(su fed:ag) nic+so ~)
          ::
          public+(ot add+bo cir+circ ~)
      ==
    ::
    ++  parse-chat-action
      =,  dejs:format
      %-  of
      :~
        [%actions (ot lis+(ar hall-action) ~)]
      ==
    ::
    --
  --
--
