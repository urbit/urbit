::
=>  %=    .
        + 
      =>  +
      |%
      ++  ut                                                  ::  queue
        |_  a=(qeu)
        +-  bal
          |-  ^+  a
          ?.  |(?=(~ l.a) (vor n.a n.l.a))
            $(a [n.l.a l.l.a $(a [n.a r.l.a r.a])])
          ?.  |(?=(~ r.a) (vor n.a n.r.a))
            $(a [n.r.a $(a [n.a l.a l.r.a]) r.r.a])
          a
        ::
        +-  dep
          |-  ^-  @
          ?~  a  0
          +((max $(a l.a) $(a r.a)))
        ::
        +-  gas
          |=  b=(list _?>(?=(^ a) n.a))
          |-  ^+  a
          ?~(b a $(b t.b, a (put i.b)))
        ::
        +-  get
          |-  ^+  [p=?>(?=(^ a) n.a) q=a]
          ?~  a
            !!
          ?~  r.a
            [n.a l.a]
          =+  b=$(a r.a)
          :-  p.b
          ?:  |(?=(~ q.b) (vor n.a n.q.b))
            [n.a l.a q.b]
          [n.q.b [n.a l.a l.q.b] r.q.b]
        ::
        +-  nap
          ?>  ?=(^ a)
          ?:  =(~ l.a)  r.a
          =+  b=get(a l.a)
          bal(a ^+(a [p.b q.b r.a]))
        ::
        +-  put
          |*  b=*
          |-  ^+  a
          ?~  a
            [b ~ ~]
          bal(l.a $(a l.a))
        ::
        +-  tap
          |=  b=(list _?>(?=(^ a) n.a))
          ^+  b
          ?~  a
            b
          $(a r.a, b [n.a $(a l.a)])
        ::
        +-  top
          |-  ^-  (unit _?>(?=(^ a) n.a))
          ?~  a  ~
          ?~(r.a [~ n.a] $(a r.a))
        --
      --
    ==
|=  [est=time eny=@uw]
|=  ~
^-  bowl
:_  ~
=<  main
=|  que=(qeu ,@)
=+  [num=65.536 mak=0b1010.0110.1010.1010]
|%
++  delt
  |=  inx=@
  |-  ^+  que
  ?~  que  !!
  ?:  =(n.que inx) 
    ~(nap ut que)
  ?:  (gth inx n.que)
    [n.que $(que l.que) r.que]
  [n.que l.que $(que r.que)]  
::
++  fill
  ^+  .
  =+  inx=0
  |-  ^+  ..fill
  ?:  =(inx num)  ..fill
  $(inx +(inx), que (~(put ut que) inx))
::
++  lose
  ^+  .
  =+  inx=0
  |-  ^+  ..lose
  ?:  =(inx num)  ..lose
  =.  que  (delt (mix inx mak))
  ?>  good
  $(inx +(inx))
::
++  good
  |-  ^-  ?
  ?~  que  &
  ?&  ?|  ?=(~ l.que)
          ?&  (lth n.que n.l.que)
              (vor n.que n.l.que)
          ==
      ==
      ?|  ?=(~ r.que)
          ?&  (gth n.que n.r.que)
              (vor n.que n.r.que)
          ==
      ==
  ==
::
++  main
  ~&  %testing
  =>  fill
  ~&  [%qt num ~(dep ut que)]
  ?>  good
  =>  lose
  ?>  good
  :~  [%la %leaf "testing complete!"]
  ==
--
