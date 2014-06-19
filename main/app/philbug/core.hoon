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
++  page
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
      ;p: Hello.
      ;table#cont  ;tbody
          ;*  %+  turn
                %+  sort  (~(tap by p.vat) ~)
                |=  [p=[u=@p n=@ud] q=[u=@p n=@ud]]  (gth n.p n.q)
              |=  [u=@p n=@ud]
              ;tr(id (slag 1 <u>)):(td:(-<u>) td:(-<n>))
      ==  ==
      ;p: Select a ship
      ;button(onclick "goof()"): Give 5 points
    ==
  ==
:: 
++  peer
  |=  [ost=bone you=ship pax=path]
  ^-  [(list move) _+>]
  ?:  =(~ pax)
    =.  p.vat  (~(put by p.vat) you (fall (~(get by p.vat) you) _@ud))
    [[ost %give %rust %hymn page]~ +>]
  :_  +>
  ~[(send-vat ost (turn (~(tap by p.vat)) |=([p=@p q=@ud] p)))]
::
++  poke-json
  |=  [ost=bone you=ship jon=json]
  ^-  [(list move) _+>]
  ~&  [%poke [%state p.vat] ost you]
  =+  j=(,[%a p=(list ,[%s p=@t])] jon)
  =.  p.vat
      %-  ~(tur by p.vat)
      |=  [u=@p n=@ud]
      ?.  (lien p.j |=([%s p=@t] =((slav %p (cat 3 '~' p)) u)))
        n
      (add 5 n)
  :_  +>+
  :-  [ost %give %rasp %json jon]
  %+  turn
    ^-  (list bone)
    %+  ~(rep by sup.hid)  *(list bone)
    |=  [p=[p=bone q=[ship path]] q=(list bone)]  ^-  (list bone)
    ?.  =(/goof +.q.p)  q
    [p.p q]
  |=  o=bone
  %+  send-vat  o
  %+  turn  p.j
  |=  [%s p=@t]
  (slav %p (cat 3 '~' p))
++  send-vat
  |=  [o=bone l=(list ,@p)]
  :*  o  %give  %rust  %json  %o
    ^-  (map ,@t jval)
    %-  mo
    %+  turn  l
    |=  p=@p
    :-  (rsh 3 1 (scot %p p))  :-  %n
    %^  rsh  3  2
    (scot %ui (fall (~(get by p.vat) p) _@ud))
  ==
--
