!:
=>  |%
    ++  perm
      $%  [%all ~]
          [%list p=(list ,@p) q=?]
      ==
    ++  axle
      $%  [%0 p=(map key stat)]
      ==
    ++  comm
      $%  [%change chan=(list key) newchan=key]
          [%update chan=(list key) sta=stat]
      ==
    ++  gilt
      $%  [%json p=json]
          [%hymn p=manx]
      ==
    ++  gift
      $%  [%lean ~]
          [%mean p=(unit ,[p=term q=(list tank)])]
          [%nice ~]
          [%rust gilt]
          [%verb ~]
      ==
    ++  hasp  ,[p=ship q=term]
    ++  key  ,@tas
    ++  move  ,[p=bone q=mess]
    ++  mess
      $%  [%give p=gift]
          [%pass p=path q=note]
      ==
    ++  note
      $?  $:  %a
              $%  [%want p=sock q=path r=*]
          ==  ==
          $:  %e
              $%  [%wart p=sock q=@tas r=path s=*]
          ==  ==
          $:  %g
              $%  [%mess p=hasp q=ship r=cage]
      ==  ==  ==
    ++  sign
      $%  $:  %a
              $%  [%send p=lane q=@]
                  [%went p=ship q=cape]
          ==  ==
          $:  %g
              $%  [%rasp (unit (pair logo noun))]
      ==  ==  ==
    ++  stat  ,[p=perm q=value]
    ++  value
      $%  [%list p=(list value)]
          [%text p=@t]
          [%map p=(map key stat)]
      ==
    --
|_  [hid=hide vat=axle]
++  incl
  |=  wal=wall
  %+  turn  wal
  |=  tape  ;script(type "text/javascript", src +<);
::
++  page
  ^-  manx
  ;html
    ;head
      ;title: Foobug!
      ;style
        ; #cont {border-collapse:  collapse; right: 0px}
        ; #news, .status {
        ;   left: 0px;
        ;   right: 0px;
        ;   background: blue;
        ;   color: white;
        ;   min-width: 100px;
        ;   min-height: 18px;
        ; }
        ; #news {
        ;   display: inline-block;
        ; }
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
      ;table#cont:tbody;
      ;button#newb: New Channel
      ;div#news(contenteditable "true");
      ;p: Enter your current status
      ;div#status(contenteditable "true");
    ==
  ==
:: 
++  peer
  |=  [ost=bone you=ship pax=path]
  ^-  [(list move) _+>]
  ?:  =(~ pax)
    [[ost %give %rust %hymn page]~ +>]
  :_  +>
  [ost msg]~
::
++  parse-acl
  |=  acl=@t  ^-  perm
  ?:  =(%everyone acl)
    [%all ~]
  [%list (fall (rush acl (more com ;~(pfix sig fed:ag))) ~) &]
::
++  poke-json
  |=  [ost=bone you=ship jon=json]
  ^-  [(list move) _+>]
  ~&  [%poke [%state p.vat] ost you jon]
  =+  ^=  sta   %-  %-  hard  ,[%o p=(map ,@t jval)]  jon
  =+  ^=  chan  %-  %-  hard  ,[~ %s p=@t]            (~(get by p.sta) %chan)
  =+  ^=  newc  %-  %-  soft  ,[~ %s p=@t]            (~(get by p.sta) %newc)
  =+  ^=  acl   %-  %-  soft  ,[~ %s p=@t]            (~(get by p.sta) %acl)
  =+  ^=  val   %-  %-  soft  ,[~ p=jval]             (~(get by p.sta) %value)
  =+  ^=  pchan
      ^-  (list key)
      (turn `(list tape)`(rash p.chan (more fas (plus ;~(pose low hep)))) crip)
  ?~  newc
    ?~  val  !!
    ?~  acl  !!
    %^  poke-stat  ost  you  :+  %update  pchan
    |-  ^-  stat
    :-  (parse-acl p.u.acl)
    ?+  -.p.u.val  !!
        %a
      [%list (turn p.p.u.val |=(j=jval q:^$(p.u.val j)))]
        %o
      :-  %map
      %+  ~(rep by p.p.u.val)  *(map key stat)
      |=  [p=[p=@t q=jval] q=(map key stat)]
      (~(put by q) p.p ^$(p.u.val q.p))
        %s
      [%text p.p.u.val]
    ==
  (poke-stat ost you [%change pchan p.u.newc])
::
++  poke-stat
  |=  [ost=bone you=ship com=comm]
  =.  p.vat
    |-  ^-  (map key stat)
    ?<  ?=(~ chan.com)
    ?.  ?=(~ t.chan.com)
      %+  ~(put by p.vat)  i.chan.com
      =+  ^=  cur
          %-  fall  :_  [~ p=all// %map q=*(map key stat)]
          %-  (soft ,[~ p=perm %map q=(map key stat)])
          (~(get by p.vat) i.chan.com)
      :-  p.cur
      :-  %map
      %=  $
        chan.com  t.chan.com
        p.vat     q.cur
      ==
    ?-  -.com
      %update  (~(put by p.vat) i.chan.com sta.com)
      %change
        %-  %~  del  by
            %+  ~(put by p.vat)  newchan.com
            (fall (~(get by p.vat) i.chan.com) *stat)
        i.chan.com
    ==
  :_  +>.$
  :*  ^-  move
::      :*  ost  %pass  /howdy  %a  %want  [our.hid our.hid]  /r/pc
::          'hi'
::      ==
::      `move`[ost %pass /hi %g %mess [~zod %appbug-2] ~zod %hi !>(~)]
      :^  ost  %give  %mean  ~
      (send /status msg)
  ==
::
++  pour
  |=  [way=path sih=sign]
  ^-  [(list move) _+>]
  ~&  [%status-pour sih]
  [~ +>]
::
++  pull
  |=  ost=bone
  ~&  [%status-pull ost]
  [~ +>.$]
::
++  root
  /(scot %p our.hid)/main/(scot %da lat.hid)/app/[app.hid]
::
++  send
  |=  [pax=path msg=mess]
  %-  turn  :_  |=(ost=bone [ost msg])
  =-  ~&  [%bones -]  -
  ^-  (list bone)
  %+  ~(rep by sup.hid)  *(list bone)
  |=  [p=[p=bone q=[ship path]] q=(list bone)]  ^-  (list bone)
  ?.  =(pax +.q.p)  q
  [p.p q]
::
++  msg
  ^-  mess
  :*  %give  %rust  %json  %o
      %-  mo
      %+  turn  (~(tap by p.vat))
      |=  [p=key q=stat]
      :-  p
      |-
      ?-  -.q.q
          %list
        [%a (turn p.q.q |=(v=value ^$(q.q v)))]
          %map
        :-  %o
        %.  :+  %acl  %s
            ?-  -.p.q
              %all   %everyone
              %list
                %+  roll  p.p.q
                |=  [p=@p q=@t]
                (cat 3 (cat 3 q ',') (scot %p p))
            ==
        %~  put  by 
        ^-  (map ,@t jval)
        %+  ~(rep by p.q.q)  *(map ,@t jval)
        |=  [p=[p=key q=stat] q=(map ,@t jval)]
        (~(put by q) p.p ^$(q q.p))
          %text
        [%s p.q.q]
      ==
  ==
--
