::
::  Markdown in Hoon.
::
=<
  |%
  ++  markdown                                          ::  block elements
    $&  [p=markdown q=markdown]
    $%  [%$ p=markline]                                 ::  plain text
        [%cobl p=(list ,@t)]                            ::  code block
        [%head p=head-level q=markline]                 ::  heading
        [%html p=manx]                                  ::  inline html
        [%list p=(unit list-order) q=(list markdown)]   ::  un/ordered list
        [%quot p=markdown]                              ::  block quote
    ==
  ++  markline                                          ::  inline elements
    $&  [p=markline q=markline]
    $%  [%$ p=@t]                                       ::  plain text
        [%bold p=markline-bold]                         ::  strong emphasis
        [%coli p=@t]                                    ::  inline code
        [%emph p=markline-emph]                         ::  emphasis
        [%link p=purl q=markline-link r=(unit ,@t)]     ::  anchor
    ==
  --
|%
++  head-level
  |=  a=*
  ?@  a
    ?:(|((lth a 1) (gth a 6)) 1 a)
  1
++  list-order  ,[p=?(%a %aa %1) q=@]                   ::  list type/start
++  markline-bold
  $&  [p=markline-bold q=markline-bold]
  $%  [%$ p=@t]
      [%coli p=@t]
      [%emph p=markline-bold-emph]
      [%link p=purl q=markline-bold-link r=(unit ,@t)]
  ==
++  markline-emph
  $&  [p=markline-emph q=markline-emph]
  $%  [%$ p=@t]
      [%bold p=markline-bold-emph]
      [%coli p=@t]
      [%link p=purl q=markline-emph-link r=(unit ,@t)]
  ==
++  markline-link
  $&  [p=markline-link q=markline-link]
  $%  [%$ p=@t]
      [%bold p=markline-bold-link]
      [%coli p=@t]
      [%emph p=markline-emph-link]
  ==
++  markline-bold-emph
  $&  [p=markline-bold-emph q=markline-bold-emph]
  $%  [%$ p=@t]
      [%coli p=@t]
      [%link p=purl q=markline-bold-emph-link r=(unit ,@t)]
  ==
++  markline-bold-link
  $&  [p=markline-bold-link q=markline-bold-link]
  $%  [%$ p=@t]
      [%coli p=@t]
      [%emph p=markline-bold-emph-link]
  ==
++  markline-emph-link
  $&  [p=markline-emph-link q=markline-emph-link]
  $%  [%$ p=@t]
      [%bold p=markline-bold-emph-link]
      [%coli p=@t]
  ==
++  markline-bold-emph-link
  $&  [p=markline-bold-emph-link q=markline-bold-emph-link]
  $%  [%$ p=@t]
      [%coli p=@t]
  ==
--
