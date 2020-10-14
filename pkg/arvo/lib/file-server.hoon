/-  sur=file-server
^?
=<  [sur .]
=,  sur
|%
++  enjs
  =,  enjs:format
  |%
  ++  update
    |=  upd=^update
    ^-  json
    |^  (frond %file-server (pairs ~[(encode upd)]))
    ::
    ++  encode
      |=  upd=^update
      ^-  [cord json]
      ?-  -.upd
          %configuration
        =*  prefix  landscape-homepage-prefix.configuration.upd
        :-  %configuration
        %-  pairs
        [%landscape-homepage-prefix ?~(prefix ~ s+u.prefix)]~
      ==
    --
  --
--
