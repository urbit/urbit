/-  collections
/+  rekey, old-zuse, colls
/=  gas  /$  fuel:html
/=  config  /collections-config/
/=  tops  /^  (map knot topicshow:collections)
          /_  /.  /collections-topic-full/
                  /=  sni  /&  collections-snip
                           /&  elem 
                           /&  md
                           ::  this needs to be put into a mar
                           /;  |=  a/topic:collections 
                               (of-wain:format wat.a)
                  /collections-topic/
              ==
~&  [%tops tops]
^-  manx
=,  old-zuse
;div.collection-index
  ;h1: {(trip desc.config)}
  ;*  ?:  (authed:colls gas)
    ;=
      ;div.row
        ;a(href "/~~/pages/nutalk/collection/post?coll={(trip -.s.bem.gas)}")
          ;button.btn.btn-secondary
            ; Write →
          ==
        ==
        ;a.ml-4.mt-2.text-600(href "")
            ; Settings →
        ==
      ==
    ==
  ;=
    ;div(urb-component "Subscribe", urb-circle "{(scow %p p.bem.gas)}/collection_~{(trip -:s.bem.gas)}");
  ==
  ;ul
    ;*  %+  turn
          %+  sort
            ~(tap by tops)
          |=  [a=(pair knot topicshow:collections) b=(pair knot topicshow:collections)]
          (dor:colls p.a p.b)
        |=  [t=knot topi=topicshow:collections]
        ;*  ?:  comm.config
              ;*  ?:  xeno.config
                ;li.forum.mb-8
                  ;div.text-mono
                    ; {(trip t)}
                  ==
                  ;div.h3.mt-0
                    ;a(href "{(trip -.s.bem.gas)}/{(trip t)}"): {(trip tit.info.top.topi)}
                  ==
                  ;div.who.text-mono.text-600
                    ; {(trip (scot %p who.info.top.topi))}
                  ==
                  ;div.meta-cont
                    ;div.da.text-mono(urb-component "Elapsed", urb-timestring "{(esoo:colls mod.info.top.topi)}");
                    ;div.com-count.ml-12
                      ; {(trip (scot %ud (lent ~(tap by coms.top.topi))))} comments
                    ==
                  ==
                ==
              ;li.blog.mb-8
                ;div.text-mono
                  ; {(trip t)}
                ==
                ;div.h2.mt-0
                  ;a(href "{(trip -.s.bem.gas)}/{(trip t)}") 
                  {(trip tit.info.top.topi)}
                  ==
                ==
                ;*  ?:  =(tit.info.top.topi '')
                      ;=
                        ;div.snippet
                          ;a(href "{(trip -.s.bem.gas)}/{(trip t)}") 
                          *{tal.snip.topi}
                          ==
                        ==
                      ==
                    ;=
                      ;div.snippet
                        *{tal.snip.topi}
                      ==
                    ==
              ==
            ;li.notes.mb-8
              ;div.da.text-mono(urb-component "Elapsed", urb-timestring "{(esoo:colls mod.info.top.topi)}");
              ;div.h3.mt-0.text-mono.note-uuid
                ;a(href "{(trip -.s.bem.gas)}/{(trip t)}"): {(trip t)}
              ==
              ;div.snippet
                ;div.h3
                  ; *{hed.snip.topi}
                ==
                ;div
                  ; *{tal.snip.topi}
                ==
              ==
            ==
  ==
==
