/-  collections
/+  colls
/=  gas  /$  fuel:html
/=  all-colls  /:  /===/web/collections  
            /^  (map knot collection:collections)  /_  /collections-collection/
|%
  :: is this collection on the profile?
  ++  is-pro
  |=  {a/knot coll/collection:collections}
  visi.conf.coll
--
^-  manx
;div.profile
  ;h2.text-mono: {(scow %p p.bem.gas)}
  ;div.row
    :: this should go to a url to create a new dm
    ;a(href "/~~/pages/nutalk/stream/create")
      ;button.btn.btn-secondary
        ; Message →
      ==
    ==
  ==
  :: where is this supposed to go?
  ;div.sein.text-700.text-mono
    ;a(href "")
      {(scow %p (sein:title p.bem.gas))}
    ==
  ==
  ;ul
    ;*  %+  turn
          %+  sort
            %+  skim
              ~(tap by all-colls)
            is-pro
          |=  [a=(pair knot *) b=(pair knot *)]
          (gth (unt:chrono:userlib (slav %da p.a)) (unt:chrono:userlib (slav %da p.b)))
        |=  [t=knot col=collection:collections]
        ;li.profile.mb-8
          ;div.h1.mt-0
            ;a(href "/~~/{(trip t)}") 
            ; {(trip desc.conf.col)}
            ==
          ==
          ;div.coll-meta
            ;div.row
              ;div.col-md-3.type.text-700
                :: this will change based on perm
                ;*  ?:  comm.conf.col
                      ?:  xeno.conf.col
                        ; Forum
                      ; Blog
                    ; Notes
              ==
              ;div.col-md-1.text-700: Created
              ;div.col-md-4.created.text-mono.ml-4
                ; {(trip t)}
              ==
            ==
            ;div.row
              ;div.col-md-3.post-count
                ; {(scow %ud (lent ~(tap by tops.col)))} posts
              ==
              ;div.col-md-1.text-700: Owner
              :: all collections on this ship are owned by me, no?
              ;div.col-md-4.owner.text-mono.ml-4
                ;a(href "")
                  ; {(scow %p p.bem.gas)}
                ==
              ==
            ==
          ==
        ==
  ==
==
