!:
::  Some uses of template syntax
::
%-  gist
|=  epic
^-  marl
=+  lim=;li:"second ;{a/funky."http://www.urbit.org" "link"} item"
=+  lum=;li:"third [owner: {(scow %p own)}] item"
=+  lom=;li:"fourth (;{i "italics"}) item"
=+  bar=;br;
=+  foo=`manx`i/;"foobarz"        
=+  lam=`(list manx)`[lim lum lom ~]
;=
  ;title: Hi again!
  ;
  ;p: Hello, world.
  ;p:"Hello: pos is {<pos>}"
  ;-  "Goodbye"
  ;+  ;p:"Hello again"
  ;*  ~[;p:"One" ;p:"Two" ;p:"Three"]
  ;br;
  ;p: Let's add some more stuff and just type
  ;p: of text!  It could definitely  be snappier, jowever!
  ;p: Behold a <p>aragraph with an embedded {"string"}.
  ;p: Or an embedded ;{i "tag"}.
  ;p: Or even a *{;i:"list" ;i:" of" ;i:" tags" ~},
  ;p: Sometimes %{|*(a=* [;i:"functionally " ;i:"applied" a])}.
  ;p: It's nice to just be able to use "quotes."  Maybe it works!
  ; Now, some flow at the same level or above
  ; I'm going to write some more text
  ; as the paragraph tag                             
  ; it is fun to flow  along,
  ;a."http://www.urbit.org": with
  ;
  ; ;{a."http://www.urbit.org" "and more fun links,"}
  ; or without,
  ; but not accepting any kind of trailing whitespace
  ;p
    ; We are also very good at random odd foo lists:
    ;ul
      ;li: first item
      ;*  lam
      ;li: last item
    ==
  ==
  ;p
    ; And we can also do
    ;pre
      ; preformatted text?
      ; in lines?
      ; with linebreaks!
    ==
  ==
  ::  ;script
    ::  ; alert("I am not an alert box!");
  ::  ==
==
