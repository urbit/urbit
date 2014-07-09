!:
:::::::::   /sys/down/tan/html/gate
::
=>  |%
++  down
  $&  [p=down q=down]
  $%  [%$ p=tape]
      [%code p=tape]
      [%emph p=down]
      [%inco p=tape]
      [%head p=@ud q=down]
      [%link p=tape q=tape r=(unit tape)]
      [%lord p=(list down)]
      [%lund p=(list down)]
      [%parg p=down]
      [%quot p=down]
      [%rong p=down]
      [%hrul ~]
      [%html p=tape]
  ==
--  
=>  |%
++  appd
  |=  [p=@ q=@]
  ^-  @
  (cat 3 p q)
::
++  hark
  |=  a=down
  ^-  @
  ?-    a
      [%$ *]     (rap 3 p.a)
      [%code *]  (wtag 'pre' (wtag 'code' (rap 3 p.a)))
      [%inco *]  (wtag 'code' (rap 3 p.a))
      [%head *]  (wtag (cat 3 'h' (add '0' p.a)) (hark q.a))
      [%link *]
    ?~  r.a
      :(appd '<a ' (sett 'href' q.a) '>' (rap 3 p.a) '</a>')
    ;:  appd
      '<a ' 
      (sett 'href' q.a) 
      ' ' 
      (sett 'title' u.r.a) 
      '>' 
      (rap 3 p.a) 
      '</a>'
    ==
  ::
      [%lord *]  
    (wtag 'ol' (reel (turn p.a |=(a=down (wtag 'li' (hark a)))) appd))
  ::
      [%lund *]  
    (wtag 'ul' (reel (turn p.a |=(a=down (wtag 'li' (hark a)))) appd))
  ::
      [%parg *]  (wtag 'p' (hark p.a))
      [%quot *]  (wtag 'blockquote' (hark p.a))
      [%rong *]  (wtag 'strong' (hark p.a))
      [%emph *]  (wtag 'em' (hark p.a))
      [%hrul *]  '<hr>'
      [%html *]  (rap 3 p.a)
      ^  (cat 3 (hark p.a) (hark q.a))
  ==
::
++  wtag
  |=  [a=@ b=@]
  ^-  @
  :(appd '<' a '>' b '</' a '>')
::
++  sett
  |=  [a=@ b=tape]
  ^-  @
  :(appd a '="' (rap 3 b) '"')
--
|=  a=down
:(appd '<html><body>' (hark a) '</body></html>')
