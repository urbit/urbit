/-  collections
/+  rekey, old-zuse, colls
/=  gas  /$  fuel:html
/=  topics  /^  (map knot topicful:collections)  /_  /collections-topic-full/  
::
:: TODO clean this up and make this one map
/=  snips    /^  (map knot {hed/marl tal/marl})  /_  
             /&  collections-snip
             /&  elem 
             /&  md
             ::  this needs to be put into a mar
             /;  |=  a/topic:collections 
                 (of-wain:format wat.a)
             /collections-topic/
/=  config  /^  config:collections  /%  /&collections-config&/txt/
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
    ;div(data-component "Subscribe", data-circle "{(scow %p p.bem.gas)}/collection_~{(trip -:s.bem.gas)}");
  ==
  ;ul
    ;*  %+  turn
          %+  sort
            ~(tap by topics)
          |=  [a=(pair knot topicful:collections) b=(pair knot topicful:collections)]
          (dor:colls p.a p.b)
        |=  [t=knot con=topicful:collections]
        =/  snip  (~(got by snips) t)
        ;*  ?:  comm.config
              ;*  ?:  xeno.config
                ;li.forum.mb-8
                  ;div.text-mono
                    ; {(trip t)}
                  ==
                  ;div.h3.mt-0
                    ;a(href "/~~/collections/{(trip -.s.bem.gas)}/{(trip t)}"): {(trip tit.info.con)}
                  ==
                  ;div.who.text-mono.text-600
                    ; {(trip (scot %p who.info.con))}
                  ==
                  ;div.meta-cont
                    ;div.da.text-mono(data-urb-elapsed "{(esoo:colls mod.info.con)}");
                    ;div.com-count.ml-12
                      ; {(trip (scot %ud (lent ~(tap by coms.con))))} comments
                    ==
                  ==
                ==
              ;li.blog.mb-8
                ;div.text-mono
                  ; {(trip t)}
                ==
                ;div.h2.mt-0
                  ;a(href "/~~/collections/{(trip -.s.bem.gas)}/{(trip t)}") 
                  *{hed.snip}
                  ==
                ==
                ;*  ?~  hed.snip
                      ;=
                        ;div.snippet
                          ;a(href "/~~/collections/{(trip -.s.bem.gas)}/{(trip t)}") 
                          *{tal.snip}
                          ==
                        ==
                      ==
                    ;=
                      ;div.snippet
                        *{tal.snip}
                      ==
                    ==
              ==
            ;li.notes.mb-8
              ;div.da.text-mono(data-urb-elapsed "{(esoo:colls mod.info.con)}");
              ;div.h3.mt-0.text-mono.note-uuid
                ;a(href "/~~/collections/{(trip -.s.bem.gas)}/{(trip t)}"): {(trip t)}
              ==
              ;div.snippet
                ::{(trip (of-wain:format (scag 3 (no-title:colls wat.info.con))))}
                ;div.h3
                  ; *{hed.snip}
                ==
                ;div
                  ; *{tal.snip}
                ==
              ==
            ==
  ==
  ;script@"/~~/pages/elapsed.js";

==
