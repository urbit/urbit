  ::  /lib/text
::::
::  Provides common text conversion routines targeted towards beginners.
::  See also https://docs.urbit.org/language/hoon/reference/zuse/2d_7,
::  which require head tags on JSON-style values.
::
::  This file is not subject to kelvin versioning and the interface should
::  not be considered official.
::
|%
::  Conversions from atom to text.
++  from-da      |=(a=@da `@t`(scot %da a))
++  from-dr      |=(a=@dr `@t`(scot %dr a))
++  from-p       |=(a=@p `@t`(scot %p a))
++  from-rd      |=(a=@rd `@t`(scot %rd a))
++  from-rh      |=(a=@rh `@t`(scot %rh a))
++  from-rq      |=(a=@rq `@t`(scot %rq a))
++  from-rs      |=(a=@rs `@t`(scot %rs a))
++  from-ub      |=(a=@ub `@t`(scot %ub a))
++  from-ud      |=(a=@ud `@t`(scot %ud a))
++  from-ux      |=(a=@ux `@t`(scot %ux a))
++  from-path    spat
++  from-number  |=(a=@rd `@t`(crip (r-co:co (drg:rd a))))
++  from-int     |=(a=@ud `@t`(crip (a-co:co a)))
::  Conversions from text to atom.
++  to-da      |=(a=@t `@da`(slav %da a))
++  to-dr      |=(a=@t `@dr`(slav %dr a))
++  to-p       |=(a=@t `@p`(slav %p a))
++  to-ub      |=(a=@t `@ub`(slav %ub a))
++  to-ud      |=(a=@t `@ud`(slav %ud a))
++  to-ux      |=(a=@t `@ux`(slav %ux a))
++  to-rd      |=(a=@t `@rd`(slav %rd a))
++  to-rh      |=(a=@t `@rh`(slav %rh a))
++  to-rq      |=(a=@t `@rq`(slav %rq a))
++  to-rs      |=(a=@t `@rs`(slav %rs a))
++  to-path    stab
++  to-number  |=(=cord `@rd`(ne:dejs:format [%n cord]))
++  to-int     |=(=cord `@ud`(rash cord dem))
::  Aliases
++  from-tape  crip
++  to-tape    trip
--
