=>  %1  =>                         ::
~%  %v.1  ~  ~                     ::  0. /v.1
|%                                 ::
++  vial-version  +                ::
++  via                            ::  1. /via/v.1
  ~/  %via                         ::
  |=  v=@  ^-  @  +(v)             ::
--  =>                             ::
~%  %one  +  ~                     ::  2. /one/v.1
|%                                 ::
++  fat                            ::  3. /fat/one/v.1
  ~/  %fat                         ::
  |=  [a=* b=*]  42                ::
++  inc                            ::  4. /inc/one/v.1
  ~/  %inc                         ::
  |=  a=@  ^-  @  +(a)             ::
--                                 ::
~%  %two  +  ~                     ::  5. /two/one/v.1
|%                                 ::
++  foo                            ::  6. /foo/two/one/v.1
  ~%  %foo  +  ~                   ::
  |_  f=@                          ::
    ++  baz                        ::  7. /baz/foo/two/one/v.1
    ~%  %baz  +  ~                 ::
    |_  z=@                        ::
      ++  lay                      ::  8. /lay/baz/foo/two/one/v.1
      ~/  %lay                     ::
      |=  y=@  ^-  ^  [+(f) +(z)]  ::
    --                             ::
  --                               ::
++  bar                            ::  9. /bar/two/one/v.1
  ~/  %bar                         ::
  |=  d=@  ^-  @  +(d)             ::
++  buz                            ::  9. /buz/two/one/v.1
  ~/  %buz                         ::
  |=  u=@  ^-  @  +(u)             ::
--                                 ::
