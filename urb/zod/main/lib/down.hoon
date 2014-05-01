::
::  Hoon Markdown tile.
::
::  It's (hopefully) impossible to construct an invalid down. The
::  l[oi][nb][ea] Matrioshka hierarchy uses self-describing names:
::  presence of 'i' means we're in italics (em), 'b' means we're
::  in bold (strong), and 'a' means we're in an anchor (a). lone
::  is in none; liba is in all.
::
|%
++  down  $&  [p=down q=down]                           ::  markdown
          $%  [%$ p=lone]                               ::  paragraph
              [%cobl p=(list ,@t)]                      ::  code block
              [%head p=lone]                            ::  heading
              [%html p=manx]                            ::  inline html
              [%list p=(unit inca) q=(list down)]       ::  un/ordered list
              [%quot p=down]                            ::  block quote
              [%rule ~]                                 ::  horizontal rule
          ==                                            ::
++  inca  ,[p=?(%a %aa %1) q=@]                         ::  list type/start
++  lone  $&  [p=lone q=lone]                           ::  inline markdown
          $%  [%$ p=@t]                                 ::  plain text
              [%bold p=lobe]                            ::  strong emphasis
              [%coli p=@t]                              ::  inline code
              [%emph p=line]                            ::  emphasis
              [%link p=purl q=lona r=(unit ,@t)]        ::  hyperlink
          ==                                            ::
++  lobe  $&  [p=lobe q=lobe]                           ::  inline in %bold
          $%  [%$ p=@t]                                 ::
              [%coli p=@t]                              ::
              [%emph p=libe]                            ::
              [%link p=purl q=loba r=(unit ,@t)]        ::
          ==                                            ::
++  line  $&  [p=line q=line]                           ::  inline in %emph
          $%  [%$ p=@t]                                 ::
              [%bold p=libe]                            ::
              [%coli p=@t]                              ::
              [%link p=purl q=lina r=(unit ,@t)]        ::
          ==                                            ::
++  lona  $&  [p=lona q=lona]                           ::  inline in %link
          $%  [%$ p=@t]                                 ::
              [%bold p=loba]                            ::
              [%coli p=@t]                              ::
              [%emph p=lina]                            ::
          ==                                            ::
++  libe  $&  [p=libe q=libe]                           ::  inline in bold-emph
          $%  [%$ p=@t]                                 ::
              [%coli p=@t]                              ::
              [%link p=purl q=liba r=(unit ,@t)]        ::
          ==                                            ::
++  loba  $&  [p=loba q=loba]                           ::  inline in bold-link
          $%  [%$ p=@t]                                 ::
              [%coli p=@t]                              ::
              [%emph p=liba]                            ::
          ==                                            ::
++  lina  $&  [p=lina q=lina]                           ::  inline in emph-link
          $%  [%$ p=@t]                                 ::
              [%bold p=liba]                            ::
              [%coli p=@t]                              ::
          ==                                            ::
++  liba  $&  [p=liba q=liba]                           ::  inline in all three
          $%  [%$ p=@t]                                 ::
              [%coli p=@t]                              ::
          ==                                            ::
--
