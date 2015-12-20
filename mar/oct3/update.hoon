::                                                      ::  ::
::::  /hoon+oct3-update+mar                        ::::::  dependencies
  ::                                                    ::  ::
/?    310                                               ::  arvo
/-    oct3                                             ::  structures
/+    oct3                                              ::  libraries
[. oct3 ^oct3]
!:                                                      ::  ::
::::                                                    ::  ::  protocol
  ::                                                    ::  ::
|_  play                                                ::  game 
++  grab                                                ::  convert from
  |%
  ++  noun  play                                        ::  from %noun
  --
++  grow                                                ::  convert to
  |%
  ++  json  ^-  ^json                                   ::  to %json  
    ~!  +>-<
    ?:  ?=({$|} +>-<)
      ~!  +>-<
      ~!  p
      s+(crip p)
    =+  she=|=(ship s+(scot %p +<))
    =+  hes=|=({ship *} (she +<-))
    %-  jobe
    :~  who+s+?:(who.p %x %o) 
        plx+?~(p.sag.p ~ (she u.p.sag.p))
        plo+?~(q.sag.p ~ (she u.q.sag.p))
        aud+a+(turn (~(tap by aud.p)) hes)
        box+~(jon bo box.p)
        boo+~(jon bo boo.p)
    ==
  --
--
