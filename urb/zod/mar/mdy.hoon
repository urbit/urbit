::
::::  /hoon/core/mdy/pro
  ::
/?  314
!:
|_  [atr=(map cord cord) mud=@t]
++  atr-lines
  %+  turn  (sort (~(tap by atr)) |=([[a=@ @] [b=@ @]] (aor a b)))
  |=  [k=cord v=cord]
  (rap 3 k ': ' v ~)
::
++  atr-key  ;~(sfix (star ;~(less col prn)) col ace)
++  grow
  |%
  ++  front  atr
  ++  mime  [/text/x-markdown (taco (role txt))]
  ++  txt   ['---' (welp atr-lines '---' (lore mud))]  :: omit if empty?
  ++  md  mud
  --
++  grab
  |%
  ++  mime  |=([p=mite q=octs] (txt (lore q.q)))
  ++  noun  ,[(map cord cord) @t]
  ++  txt
    |=  wan=wain  ^+  [atr mud]
    ?~  wan  [~ '']
    ?^  (rush i.wan (star ace))
      $(wan t.wan)
    ?.  =('---' i.wan)  [~ (role wan)]
    |-  ^+  [atr mud]
    ?~  t.wan  ~|(%unclosed-metadata !!)
    ?:  =('---' i.t.wan)  [atr (role t.t.wan)]
    ?^  (rush i.t.wan (star ace))
      $(wan t.wan)
    =-  $(wan t.wan, atr (~(put by atr) (crip key) (crip val)))
    ~|  malformed-attribute/i.t.wan
    ^-  [key=tape ^ val=tape]
    +>:(atr-key 1^1 (trip i.t.wan))
  --
++  grad  %txt
--
