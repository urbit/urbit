!:
|%
++  react-elems ::  XX /~
  ~+  %-  sa  ^-  (list term)
  :~  %a  %abbr  %address  %area  %article  %aside  %audio  %b  %base
    %bdi  %bdo  %big  %blockquote  %body  %br  %button  %canvas  %caption
    %cite  %code  %col  %colgroup  %data  %datalist  %dd  %del  %details
    %dfn  %dialog  %div  %dl  %dt  %em  %embed  %fieldset  %figcaption
    %figure  %footer  %form  %h1  %h2  %h3  %h4  %h5  %h6  %head  %header
    %hr  %html  %i  %iframe  %img  %input  %ins  %kbd  %keygen  %label
    %legend  %li  %link  %main  %map  %mark  %menu  %menuitem  %meta
    %meter  %nav  %noscript  %object  %ol  %optgroup  %option  %output  %p
    %param  %picture  %pre  %progress  %q  %rp  %rt  %ruby  %s  %samp
    %script  %section  %select  %small  %source  %span  %strong  %style
    %sub  %summary  %sup  %table  %tbody  %td  %textarea  %tfoot  %th
    %thead  %time  %title  %tr  %track  %u  %ul  %var  %video  %wbr
    %circle  %defs  %ellipse  %g  %line  %linear-gradient  %mask  %path
    %pattern  %polygon  %polyline  %radial-gradient  %rect  %stop  %svg
    %text  %tspan
  ==
++  react-vale
  ~(has in react-elems)
++  react-to-json
  |=  src=manx  ^-  json
  ?:  ?=(_:/(**) src)
    (jape v.i.a.g.src)
  =+  atr=(mo a.g.src)
  ?:  (~(has by atr) [%urb %codemirror])
    ?>  ?=([[%pre *] _:/(**) ~] src)
    $(src ;codemirror(value "{v.i.a.g.i.c.src}");)
  ?:  (~(has by atr) [%urb %exec])           ::  runnable code attribute tag
    ?>  ?=([[%pre *] _:/(**) ~] src)                  ::  verify its only a text node
    =*  code  v.i.a.g.i.c.src
    %_    $
        src
      =+  =<  result=(mule .)
          !.(|.((slap !>(.) (ream (crip code)))))     ::  compile and run safely
      =+  claz=?:(-.result "rancode" "failedcode")
      ;div(class "{claz}")
        ;pre:"{code}"
        ;+  ?:  ?=(& -.result)
              ;code:"{~(ram re (sell p.result))}"
            ;pre
              ;div:"error"
              ;*  %+  turn  p.result
                  |=  a=tank
                  ^-  manx
                  ;div:"{~(ram re a)}"
      ==    ==
    ==
  %-  jobe  :~
    c/a/(turn c.src ..$)
    gn/s/(mane-to-cord n.g.src)
    =<  ga/(jobe (turn a.g.src .))
    |=  [a=mane b=tape]  ^-  [cord json]
    [?+(a (mane-to-cord a) %class 'className') (jape b)]
  ==
::
++  mane-to-cord  |=(a=mane `cord`?@(a a (rap 3 -.a ':' +.a ~)))
::  generates React javascript  XX deprecated
++  react-to-tape                                    
  |=  src=manx  ^-  tape
  ?:  (~(has by (mo a.g.src)) [%urb %codemirror])
    ?>  ?=([[%pre *] _:/(**) ~] src)
    $(src ;codemirror(value "{v.i.a.g.i.c.src}");)
  ?:  (~(has by (mo a.g.src)) [%urb %exec])           ::  runnable code attribute tag
    ?>  ?=([[%pre *] _:/(**) ~] src)                  ::  verify its only a text node
    =*  code  v.i.a.g.i.c.src
    =+  ^=  result
      (mule |.((slap !>(.) (ream (crip code)))))      ::  compile and run safely
    ?:  ?=(%.y -.result)                              ::  it was ok
        =+  ^=  new
          ;div(class "rancode")
                ;pre:"{code}"
                ;code:"{~(ram re (sell p.result))}"
            ==
        $(src new)
    =+  ^=  error
      ;div(class "failedcode")
        ;pre:"{code}"
        ;pre
          ;div:"error"
          ;*  %+  turn
                (scag (dec (lent p.result)) p.result) ::  hide react trace
              |=  a=tank
              ^-  manx
              ;div:"{~(ram re a)}"
        ==
      ==
    $(src error)
  ;:  weld
    "React.createElement("
      =*  tan  n.g.src
      ?^  tan  !!                                       ::  namespaces unsupported
      ?:  (react-vale tan)
        "'{(trip tan)}'"
      (trip tan)
    ", "
      =-  (pojo (jobe (turn a.g.src -)))
      |=  [a=mane b=tape]
      =.  a  ?+(a a %class 'className')
      ?^(a !! [a (jape b)])
    ", "
      =<  ~(ram re %rose [", " "[" "]"] (turn c.src .))
      |=  a=manx
      ?:  ?=(_:/(**) a)
        leaf/(pojo (jape v.i.a.g.a))
      leaf/^$(src a)
    ")"
  ==
--
