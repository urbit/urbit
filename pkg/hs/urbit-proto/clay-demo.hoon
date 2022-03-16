=/  clay-type  (map path [typ=$ : bod=typ])
=/  clay
  :>  /foo/bar  [@ 1]
      /baz      [@t 'hello world']
      /sys/hoon/hoon  [(list tape)  ~["=>  %140  %implement-later"]]
  ==
^-  @
bod:(by clay get: /foo/bar)  ::  = 1
