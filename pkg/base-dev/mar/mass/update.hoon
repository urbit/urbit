/-  *mass
|_  upd=update
++  grow
  |%
  ++  noun  upd
  ++  json
    ^-  ^json
    =,  enjs:format
    ?-    upd
        [%new *]
      %+  frond  'new'
      %-  pairs
      :~  ['time' s+(scot %da time.upd)]
          ['path' a+(turn path.upd (lead %s))]
          ['size' (numb size.upd)]
          ['base' s+(scot %uv base.upd)]
      ==
    ::
        [%new-all *]
      %+  frond  'newAll'
      %-  pairs
      :~  ['time' s+(scot %da time.upd)]
          :+  'data'  %a
          %+  turn  ~(tap by data.upd)
          |=  [path=(list @t) size=@ud base=@uvI]
          a+[a+(turn path (lead %s)) (numb size) s+(scot %uv base) ~]
      ==
    ::
        [%old *]
      %+  frond  'old'
      %-  pairs
      :~  ['path' a+(turn path.upd (lead %s))]
          :+  'list'  %a
          %+  turn  list.upd
          |=  [t=@da size=@ud base=@uvI]
          a+[s+(scot %da t) (numb size) s+(scot %uv base) ~]
      ==
    ::
        [%old-all *]
      %+  frond  'oldAll'
      :-  %a
      %+  turn  ~(tap by data.upd)
      |=  [path=(list @t) list=(list (trel @da @ud @uvI))]
      %-  pairs
      :~  ['path' a+(turn path (lead %s))]
          :+  'list'  %a
          %+  turn  list
          |=  [t=@da size=@ud base=@uvI]
          a+[s+(scot %da t) (numb size) s+(scot %uv base) ~]
      ==
    ::
        [%newest *]
      ?~  p.upd  ~
      %-  pairs
      :~  ['time' s+(scot %da time.u.p.upd)]
          ['path' a+(turn path.u.p.upd (lead %s))]
          ['size' (numb size.u.p.upd)]
          ['base' s+(scot %uv base.u.p.upd)]
      ==
    ::
        [%raw *]
      ?~  p.upd  ~
      %-  pairs
      :~  ['time' s+(scot %da time.u.p.upd)]
          ['base' s+(scot %uv base.u.p.upd)]
          :+  'quacs'  %a
          |-
          ?~  quacs.u.p.upd
            ~
          :-  %-  pairs
              :~  ['name' s+name.i.quacs.u.p.upd]
                  ['size' (numb size.i.quacs.u.p.upd)]
                  ['quacs' %a $(quacs.u.p.upd quacs.i.quacs.u.p.upd)]
              ==
          $(quacs.u.p.upd t.quacs.u.p.upd)
      ==
    ::
        [%poll *]
      ?~  every.upd  ~
      ?@  u.every.upd
        s+(scot %dr u.every.upd)
      :-  %a
      :~  (numb d.u.every.upd)
          (numb h.u.every.upd)
          (numb m.u.every.upd)
      ==
    ::
        [%acl *]
      ?~  p.upd  ~
      %-  pairs
      :~  ['add' a+(turn ~(tap in add.u.p.upd) |=(=@p s+(scot %p p)))]
          ['rm' a+(turn ~(tap in rm.u.p.upd) |=(=@p s+(scot %p p)))]
      ==
    ==
  --
++  grab
  |%
  ++  noun  update
  --
++  grad  %noun
--
