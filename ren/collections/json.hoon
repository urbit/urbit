/?  309
/+  collections, cram, elem-to-react-json
/=  gas  /$  fuel:html
/=  jon
/^  json
/;  =<  item-to-json
    |%
    ::
    ++  item-to-json
      |=  itm=item:collections
      ^-  json
      ?-    -.itm
      ::
          %collection
        %+  frond:enjs:format
        %collection  (collection-to-json col.itm)
      ::
          %raw
        %-  frond:enjs:format
        [%item (raw-to-json raw.itm)]
      ::
          %both
        %-  pairs:enjs:format
        :~  [%item (raw-to-json raw.itm)]
            [%collection (collection-to-json col.itm)]
        ==
      ==
    ::
    ++  collection-to-json
      |=  col=collection:collections
      ^-  json
      %-  pairs:enjs:format
      :~  [%meta (config-to-json meta.col)]
          :+  %data  %a
          %+  turn  ~(tap by data.col)
          |=  [nom=knot ite=item:collections]
          ^-  json
          %-  pairs:enjs:format
          :~  [%filename %s nom]
              [%item (item-to-json ite)]
          ==
      ==
    ::
    ++  raw-to-json
      |=  raw=raw-item:collections
      ^-  json
      =/  elm=manx  elm:(static:cram (ream data.raw))
      =/  rec=json  (elem-to-react-json elm)
      %-  pairs:enjs:format
      :~  [%data rec]
          [%meta (meta-to-json meta.raw)]
      ==
    ::
    ++  config-to-json
      |=  con=config:collections
      ^-  json
      ?:  =(con *config:collections)
        ~
      %-  pairs:enjs:format
      :~  :-  %full-path
            :-  %a
            %+  turn  (en-beam:format full-path.con)
            |=  a=@ta
            [%s a]
          :-  %name           [%s name.con]
          :-  %desc           [%s description.con]
          :-  %owner          (ship:enjs:format owner.con)
          :-  %date-created   (time:enjs:format date-created.con)
          :-  %last-modified  (time:enjs:format last-modified.con)
          :-  %type           [%s type.con]
          :-  %comments       [%b comments.con]
          :-  %sort-key       ?~(sort-key.con ~ (numb:enjs:format u.sort-key.con))
          :-  %visible        [%b visible.con]
      ==
    ::
    ++  meta-to-json
      |=  meta=(map knot cord)
      ^-  json
      %-  pairs:enjs:format
      %+  turn  ~(tap by meta)
      |=  [key=@t val=@t]
      ^-  [@t json]
      [key [%s val]]
    ::
    --
::
/%  /collections-web-item/
::
jon
