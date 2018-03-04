::  "Hello world" sample generator  
::
::::  /hoon/hello/gen
  ::
/?    310
::
::::
  ::
!:
^%
:-  %say
|=  *
=>  |%
    ++  pki  ^?
      |%
      ::  the urbit meta-certificate (++will) is a sequence
      ::  of certificates (++cert).  each cert in a will
      ::  revokes and replaces the previous cert.  the
      ::  version number of a ship is a ++life.
      ::
      ::  the deed contains an ++arms, a definition
      ::  of cosmetic identity; a semi-trusted parent,
      ::  which signs the initial certificate and provides
      ::  routing services; and a dirty bit.  if the dirty
      ::  bit is set, the new life of this ship may have
      ::  lost information that the old life had.
      ::
      ++  hair
        |*  {a/mold b/mold}
        $:  one/a
            two/b
        ==
      --  ::  pki
    --
:-  %noun
"hello, world"
