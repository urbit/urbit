::    this mark is used to receive a confirmed transaction
::
::::  /hoon#bit-accounts#mar
  ::
/?    310
/-    bit-api
!:
[bit-api .]
|_  bit-accounts
::
++  grab                          ::  converter arm
  |%
  ++  noun  bit-accounts                  ::  clam from noun
  ++  json  |=  jon/^json  ^-  bit-accounts
            ~|  jon
            %-  need  %.  jon
            =>  jo
            =+  bal=(ot amount#so currency#(cu cass sa) ~)
            (ot accounts#(ar (ot id#so name#so balance#bal ~)) ~)
  --
--
