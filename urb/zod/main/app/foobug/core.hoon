!:
=>  |%
    ++  axle
      $%  [%0 p=@ud]   
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
++  page
  ^-  manx
  ;html
    ;head
      ;title: Foobug!
    ==
    ;body
      ;p: Word: {<p.vat>}.
      ;button(onclick "goof()"): Goof!
      ;script
        ; var c = 0;
        ; var d = 1;
        ; var x = 0;
        ; 
        ; function pickup() {
        ;   xhr = new XMLHttpRequest();
        ;
        ;   console.log("WTF???");
        ;   xhr.open("GET", "/zod/goe/foobug/"+port+"/frog/"+d);
        ;   xhr.onload = function() {
        ;     console.log("pickup");
        ;     console.log(this)
        ;     change(JSON.parse(this.responseText))
        ;     update();
        ;     d++;
        ;     pickup();
        ;   }
        ;   xhr.send();
        ; }
        ;
        ; function dude() {
        ;   xhr = new XMLHttpRequest();
        ;
        ;   xhr.open("POST", "/zod/pos/foobug/"+port+"/frog/goof");
        ;   xhr.setRequestHeader("content-type", "text/json")
        ;   xhr.onload = function() {
        ;     console.log("dude");
        ;     console.log(this)
        ;     change(JSON.parse(this.responseText))
        ;     update();
        ;     pickup();
        ;   }
        ;   xhr.send("{\"a\":1}")
        ; }
        ; dude();
        ;
        ; function change(jon) {
        ;   x = jon.x;
        ; }
        ;
        ; function update() {
        ;   document.getElementsByTagName("p")[0].innerHTML = "WORD: " + x;
        ; }
        ;
        ; function goof() {
        ;   xhr = new XMLHttpRequest();
        ;   xhr.onload = function() {
        ;     console.log("hi");
        ;     console.log(arguments)
        ;     c++
        ;   }
        ;   xhr.open("POST", 
        ;     "/zod/pom/foobug/"+port+"/"+c)
        ;   xhr.setRequestHeader("content-type", "text/json")
        ;   xhr.send("{\"a\":1}")
        ; }
      ==
    ==
  ==
:: 
++  peer
  |=  [ost=bone you=ship pax=path]
  ^-  [(list move) _+>]
  ?:  =(~ pax)
    [[ost %give %rust %hymn page]~ +>]
  :_  +>
  [ost %give %rust %json `json`(joba %x [%n (rsh 3 2 (scot %ui p.vat))])]~
::
++  poke-json
  |=  [ost=bone you=ship jon=json]
  ^-  [(list move) _+>]
  ~&  [%poke [%state p.vat] ost you jon]
  :_  +>(p.vat +(p.vat))
  :~  [ost %give %rasp %json jon]
  ==
--
