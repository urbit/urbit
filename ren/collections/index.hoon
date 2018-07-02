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
^-  manx
=,  old-zuse

;div.container
  ;input(type "hidden", name "urb-header", value "collection", title "{(trip desc.config)}", station "{(scow %p p.bem.gas)}/collection_~{(trip -.s.bem.gas)}", publ "{<publ.config>}");
  ;div.row
    ;div.col-sm-10.col-sm-offset-2
      ;div.collection-index.mt-12
        ;ul
          ;*  %+  turn
                %+  sort
                  ~(tap by tops)
                |=  [a=(pair knot topicshow:collections) b=(pair knot topicshow:collections)]
                (dor:colls p.a p.b)
              |=  [t=knot topi=topicshow:collections]
              ;*  ?:  comm.config
                    ;*  ?:  xeno.config
                      ;li.collection-post.mt-6
                        ;div.collection-date
                          ; {(trip t)}
                        ==
                        ;h3
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
                    ;li.collection-post.mt-6
                      ;div.collection-date
                        ; {(trip t)}
                      ==
                      ;h3
                        ;a(href "{(trip -.s.bem.gas)}/{(trip t)}")
                          ;span: *{hed.snip.topi}
                        ==
                      ==
                      ;*  ?~  hed.snip.topi
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
    ==
  ==
==
