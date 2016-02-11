::
::::  /hoon/markdown/sur
  ::
/?    314
|%
++  down  (list elem)
++  kids  (list inline)
++  inline
  =+  ^=  inlik
      $%  {$emph p/?}                                   ::  strong?
          {$delt $~}                                    ::  strikethrough
          {$link p/tape q/(unit tape)}
          {$blot p/tape q/(unit tape)}                  ::  image
      ==
  =+  ^=  inlin
      $%  {$$ p/tape}                                    
          {$line $~}
          {$code p/tape}
          {$htmt p/cord}                                ::  XX  (each marx mane)
      ==
  $^({p/inlik q/kids} inlin)
::
::
++  elem  $^(tops node)
++  tops                                                ::  childful block
  $:  $=  p
      $%  {$bloq $~}
          {$list p/? q/$@(char {p/@u q/char})}          ::  tight ordered?
          {$item $~}
      ==
      q/down
  ==
++  node                                                ::  childless block
  $%  {$para p/kids}
      {$meta p/(map cord cord)}                         ::  front matter
      {$hrul $~}
      {$head p/@u q/kids}
      {$code p/(unit {p/char q/@u r/tape}) q/wain}      ::  info contents
      {$html p/wain}
      {$defn $~}                                        ::  empty para
  ==
--

