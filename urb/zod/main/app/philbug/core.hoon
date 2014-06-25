!:
=>  |%
    ++  axle
      $%  [%0 p=(map ,@p ,@ud)]   
      ==
    ++  gilt
      $%  [%json p=json]
          [%hymn p=manx]
      ==
    ++  gift
      $%  [%rust gilt]
          [%rasp gilt]
      ==
    ++  move  ,[p=bone q=[%give p=gift]]
    ++  phil  
      $%  [%new ~]
          [%add p=(list ,@p)]
      ==
    --
|=  *
|_  [hid=hide vat=axle]
++  incl
  |=  wal=wall
  %+  turn  wal
  |=  tape  ;script(type "text/javascript", src +<);
::
++  root
  /(scot %p our.hid)/main/(scot %da lat.hid)/app/[app.hid]
::
++  respond                                             ::  respond to message
  |=  ost=bone
  `move`[ost %give %rasp %json *json]
::
++  update                                              ::  update subscribers
  ^-  (list move)
  %+  turn
    ^-  (list bone)
    %+  ~(rep by sup.hid)  *(list bone)
    |=  [p=[p=bone q=[ship path]] q=(list bone)]  ^-  (list bone)
    ?.  =(/goof +.q.p)  q
    [p.p q]
  send-vat
::
++  render
  ^-  manx
  ;html
    ;head
      ;title: Foobug!
      ;style
        ; .sel {background:  lightgray}
        ; #cont {border-collapse:  collapse}
      ==
      ;*  %-  incl  :~
        "//cdnjs.cloudflare.com/ajax/libs/jquery/2.1.1/jquery.min.js"
      ==
      ;script  ;-  (trip ;;(,@ .^(%cx (welp root /urb/js))))
      ==
      ;script  ;-  (trip ;;(,@ .^(%cx (welp root /app/js))))
      ==
    ==
    ;body
      ;p: Yo.
      ;table#cont:tbody;
      ;p: Select a ship
      ;button(onclick "goof()"): Give 5 points
    ==
  ==
:: 
++  peer
  |=  [ost=bone you=ship pax=path]
  ^-  [(list move) _+>]
  :_  +>
  ?:  =(/ pax)
    [ost %give %rust %hymn render]~
  [(send-vat ost) ~]
::
++  poke-phil
  |=  [ost=bone you=ship pil=phil]
  =.  p.vat 
      ?-  -.pil
        %new  (~(put by p.vat) you (fall (~(get by p.vat) you) _@ud))
        %add  %-  ~(urn by p.vat)
              |=  [u=@p n=@ud]
              ?.  (lien p.pil |=(a=@p =(a u)))
                n
              (add 5 n)
      ==
  [[(respond ost) update] +>.$]
::
++  poke-json
  |=  [ost=bone you=ship jon=json]
  ~&  [%poke-json jon]
  %^  poke-phil  ost  you
  ^-  phil
  ?+  -.jon  !!
    %o  [%new ~]
    %a  :-  %add
        %+  turn  p.jon
        |=  a=json
        ?>  ?=([%s *] a)
        (slav %p (cat 3 '~' p.a))
  ==
::
++  send-vat
  |=  ost=bone
  =+  luz=(~(tap by p.vat) ~)
  ^-  move
  :*  ost  %give  %rust  %json  %o
    ^-  (map ,@t jval)
    %-  mo
    %+  turn  luz
    |=  [a=@p b=@ud]
    :-  (rsh 3 1 (scot %p a))  
    :-  %n
    (rsh 3 2 (scot %ui b))
  ==
--
