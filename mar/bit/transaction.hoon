::    this mark is used to receive a confirmed transaction
::
::::  /hoon/bit-transaction/mar
  ::
/?  310
!:
|_  id+@t  ::XX time
::
++  grab                          ::  converter arm
  |%
  ++  noun  @t                   ::  clam from noun
  ++  json  |=  jon+^json
            ~|  jon
            (need ((ot transaction/(ot id/so ~) ~):jo jon))
  --
--

