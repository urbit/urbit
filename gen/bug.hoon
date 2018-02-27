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
      ++  arms  (map chip (pair @ta @t))                  ::  stated identity
      ++  bull                                            ::  cert metadata
        $:  dad/ship                                      ::  parent
            dob/?                                         ::  & clean, | dirty
            nym/arms                                      ::  identity strings
        ==                                                ::
      ++  cert  (tale deed)                               ::  signed deed
      ++  chip                                            ::  standard identity
        $?  $giv                                          ::  given name
            $sur                                          ::  surname
            $had                                          ::  fictitious name
            $mid                                          ::  middle name
        ==                                                ::
      ++  deed                                            ::  certificate deed
        $:  doc/bull                                      ::  metadata
            pub/pass                                      ::  public key
        ==                                                ::
      ++  farm  (map ship will)                           ::  pki dump set
      ++  hand  @uvH                                      ::  128-bit hash
      ++  life  @ud                                       ::  ship version
      ++  mind  {who/ship lyf/life}                       ::  key identifier
      ++  name  (pair @ta @t)                             ::  ascii / unicode
      ++  oath  @                                         ::  signature
      ++  tale                                            ::  urbit-signed *
        |*  typ/mold                                      ::  payload mold
        $:  dat/typ                                       ::  data
            syg/(map ship (pair life oath))               ::  signatures
        ==                                                ::
      ++  will  (map life cert)                           ::  meta-certificate
      --  ::  pki
    --
:-  %noun
"hello, world"
