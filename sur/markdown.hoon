|%  
++  down  (list elem)
++  kids  (list inline)
++  inline
  =+  ^=  inlik
      _%  {$emph p+?}                                   ::  strong?
          {$delt $~}                                    ::  strikethrough
          {$link p+tape q+(unit tape)}
          {$blot p+tape q+(unit tape)}                  ::  image
      ==
  =+  ^=  inlin
      _%  {$$ p+tape}                                    
          {$line $~}
          {$code p+tape}
          {$htmt p+cord}                                ::  XX  (each marx mane)
      ==
  _&({p+inlik q+kids} inlin)
::
::
++  elem  _&(tops node)
++  tops                                                ::  childful block
  _:  _=  p
      _%  {$bloq $~}
          {$list p+? q+_|(char {p+@u q+char})}          ::  tight ordered?
          {$item $~}
      ==
      q+down
  ==
++  node                                                ::  childless block
  _%  {$para p+kids}
      {$meta p+(map cord cord)}                         ::  front matter
      {$hrul $~}
      {$head p+@u q+kids}
      {$code p+(unit {p+char q+@u r+tape}) q+wain}      ::  info contents
      {$html p+wain}
      {$defn $~}                                        ::  empty para
  ==
--

