/-  collections
/+  rekey, old-zuse, colls
/=  gas  /$  fuel:html
/=  topics  /^  (map knot topicful:collections)  /_  /collections-topic-full/  
/=  config  /^  config:collections  /%  /&collections-config&/txt/
|%
++  esoo
  |=  d/@d
  ^-  tape
  =/  t  (yore d)
  ;:  welp
      (scag 1 (scow %ud y.t))
      (swag [2 3] (scow %ud y.t))
      "-"
      (double m.t)
      "-"
      (double d.t.t)
      "T"
      (double h.t.t)
      ":"
      (double m.t.t)
      ":"
      (double s.t.t)
      "Z"
  ==
:: ud to leading zero tape
++  double
  |=  a/@ud
  ^-  tape
  =/  x  (scow %ud a)
  ?:  (lth a 10)
    (welp "0" x)
  x
--
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
    ;div.row
      ;a(href "")
        ;button.btn.btn-primary
          ; Subscribe →
        ==
      ==
    ==
  ==
  ;ul
    ;*  %+  turn
          %+  sort
            ~(tap by topics)
          |=  [a=(pair knot topicful:collections) b=(pair knot topicful:collections)]
          (dor:colls p.a p.b)
        |=  [t=knot con=topicful:collections]
        ;*  ?:  comm.config
              ;*  ?:  xeno.config
                ;li.forum
                  ;div.text-mono
                    {(trip t)}
                  ==
                  ;div.h3.mt-0
                    ;a(href "/~~/collections/{(trip -.s.bem.gas)}/{(trip t)}"): {(trip tit.info.con)}
                  ==
                  ;div.who
                    ; {(trip (scot %p who.info.con))}
                  ==
                  ;div.com-count
                    ; {(trip (scot %ud (lent ~(tap by coms.con))))} comments
                  ==
                ==
              ;li.blog
                ;div.text-mono
                  {(trip t)}
                ==
                ;div.h2.mt-0
                  ;a(href "/~~/collections/{(trip -.s.bem.gas)}/{(trip t)}"): {(trip tit.info.con)}
                ==
                ;div.snippet
                  {(trip (of-wain:format (scag 3 (no-title:colls wat.info.con))))}
                ==
              ==
            ;li.notes
              ;div.da(data-da "{(esoo mod.info.con)}");
              ;div.h3.mt-0.text-mono
                ;a(href "/~~/collections/{(trip -.s.bem.gas)}/{(trip t)}"): {(trip t)}
              ==
              ;div.snippet
                {(trip (of-wain:format (scag 3 (no-title:colls wat.info.con))))}
              ==
            ==
        ;script:'''
                var das = document.querySelectorAll('[data-da]');
                for (var i=0; i < das.length; i ++) { 
                  var urbD = das[i].dataset.da;  // UTC
                  console.log(urbD);
                  var t = new Date(new Date(urbD).toUTCString()); // local
                  var clientDate = new Date(); // local
                  document.querySelector("[data-da='" + urbD + "']").innerHTML = t - clientDate;
                }
                '''
  ==

==
