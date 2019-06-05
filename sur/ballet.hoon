::  /sour/ballot
::
::  TODO: ring signature stuff should be provided by their own sur files, this is just
::  stubs for now.
::
|%
++  ring-tag
  ::  TODO: the ring-tag is really supposed to be a point on a elliptic curve:
  ::  [@ @]. But to make sure we have ring-tag uniqueness for now, just make
  ::  the structure a ship name.
  ::
  @p
+$  ring-signature
  [@ (list @) =ring-tag]
++  verify-signature
  |=  [=ring-signature linkage-tag=(unit *) data=*]
  ^-  ?
  ~&  %verify-signature-is-a-stub
  ::
  %.y
--
|%
::  The electorate is the set of people who may vote in an election. This is
::  set during the creation of the election
::
++  electorate
  (set @p)
::  The ballot is the set of questions which are then voted on.
::
++  ballot
  $:  preamble=tape
      questions=(list question)
  ==
::  A question is either a select as many as you want %check or select one %radio
::
++  question
  $%  [%check preamble=tape descriptions=(list tape)]
      [%radio preamble=tape descriptions=(list tape)]
  ==
::  An individual answer to a question
::
++  answer
  $%  [%check checked=(list @u)]
      [%radio checked=@u]
  ==
::  A vote is a list of answers to questions on a ballot
::
++  vote
  (list answer)
::  A unique id which is used as linkage scope on signatures to votes
::
++  id
  $:  host=@p
      election-num=@u
  ==
::  A running count of the state of the election
::
+$  tally
  (list (map index=@u count=@u))
::  +election contains all the state of an election, and is releasable during
::  and after the fact
::
++  election
  $:  ::  a unique id for this election
      ::
      =id
      ::  who may vote
      ::
      =electorate
      ::  what is being voted on
      ::
      =ballot
      ::  when the election closes
      ::
      closes=@da
      ::  cast votes, sorted on linkage tag
      ::
      cast=(map ring-tag [=ring-signature =vote])
      ::  the current running tally of the results
      ::
      =tally
  ==
::  +election-diff: a series of events which rebuilds an +election object
::
++  election-diff
  $%  [%snapshot =election]
      [%vote =ring-signature =vote]
      [%election-completed ~]
  ==
--


