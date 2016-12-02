::  Possibly non-fatal http error
::
::::  /hoon/recoverable-error/mar
  ::
/-    recoverable-error
::
::::  ~fyr
  ::
=,  ^eyre
=,  format
=,  html
|_   recoverable-error
++  grab
  |%
  ++  noun  recoverable-error
  ++  httr
    |=  a/^httr  ^-  recoverable-error
    ~!  a
    ?+  p.a  ~|(non-recoverable+p.a !!)
      $429  :+  p.a  %rate-limit
            %.  %x-rate-limit-reset
            ;~  biff
              ~(get by (malt q.a))
              de-json
              ni:dejs-soft
            ==
    ==
  --
++  grow  |%  ++  tank  >[+<]<  --
--
