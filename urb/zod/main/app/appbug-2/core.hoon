!:  |=  *
:::::::::   Foobug: a simple application.
::
|_  [hid=hide vat=[%0 p=@ud]]
++  poke
  |=  [ost=bone you=ship jon=json]
  ~&  [%poke p.vat you (pojo jon)]
  :_  +>(p.vat +(p.vat))
  :~  [ost %give %rasp ~]
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
      ;p: Dude, the answer is {<p.vat>}.
      ;button(onclick "bump()"): (bump.)
      ;script
        ; var mess = 0;
        ;
        ; function bump() {
        ;   xhr = new XMLHttpRequest();
        ;   xhr.onload = function() { mess++; } 
        ;   xhr.open("POST", "/pim/"+user+"/"+appl+"/"+port+"/"+mess)
        ;   xhr.setRequestHeader("content-type", "text/json")
        ;   xhr.send(JSON.stringify({oryx: oryx, xyro: {}}));
        ; }
      ==
    ==
  ==
--


