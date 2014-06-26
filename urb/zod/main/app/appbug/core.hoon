!:  |=  *
:::::::::   Appbug: a simple application.
::
=>  |%
    ++  catt
      |=  [@t @t]
      ^-  @t
      (cat 3 +<- +<+)
    ++  clay
      |=  [mih=ship dez=span caz=span sup=path]
      =>  |%
          ::
          ++  gifted
            |=  (list miso)
            =+  (turn +< |=(p=miso [sup p]))
            [%into mih dez (nori [& ~ [*cart -]])]
          ::
          ++  plan
            |=  [chg=$+([@ @] @) den=@ dif=$+([@ @] udon)]
            ^-  (list miso)
            =+  cur=curr
            ?~  cur
              [[%ins den] ~]
            ?^  q.u.cur
              [[%del q.u.cur] [%ins den] ~]
            [[%mut (dif q.u.cur (chg q.u.cur den))] ~]
          ::
          ++  curr
            ^-  (unit ,[p=cash q=*])
            =+  yaz=(zu ((hard ankh) .^(%cz (scot %p mih) dez caz ~)))
            q:ank:(deny:yaz sup)
          --
      |%
      ++  pend  |=(den=@t (gifted (plan catt den (diff %c))))
      ++  push  |=(den=@ (gifted (plan |=(* den) den (diff %c))))
      ++  pull  (bind curr |*([cash q=*] q))
      ++  outw  |=(* (push (scot %uw (jam +<))))             :: we're not JS  XX
      ++  getw  ?.(.?(pull) ~ (cue (slav %uw ;;(,@ +.pull))))  :: ?~  fails   XX
      --
    --
|_  [hid=hide vat=[%0 p=@ud]]
++  poke
  |=  [ost=bone *]
  =.  p.vat  +(p.vat)
  :_  +>.$ 
  =+  msg=:(catt (scot %da lat.hid) ' ping ' (scot %ud p.vat))
  :~  [ost %give %rasp ~ %json *json]
      :^  ost  %pass  /no/return
      [%c (pend:(clay our.hid /try/(scot %da lat.hid)/bump/log) msg)]
  ==
::
++  peek
  |=  [you=ship pax=path]
  :-  %hymn
  ^-  manx
  ;html
    ;head
      ;title: Foobug!
    ==
    ;body
      ;p: Dude, a better answer is {<p.vat>}.
      ;button(onclick "bump()"): (Bump.)
      ;script
        ; var mess = 0;
        ;
        ; function bump() {
        ;   xhr = new XMLHttpRequest();
        ;   xhr.onload = function() { mess++; } 
        ;   xhr.open("PUT", "/tim/"+user+"/"+appl+"/"+port+"/"+mess);
        ;   xhr.setRequestHeader("content-type", "text/json");
        ;   xhr.send(JSON.stringify({oryx: oryx, xyro: {}}));
        ; }
      ==
    ==
  ==
++  haxs
  1
--


