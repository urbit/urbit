::
::::  /hoon/hello/gen
  ::
/?    310
::
::::
  !:
:-  %say
|=  {* {{txt/@tas $~} $~}}
=+  ^=  foo
    ^-  tape
    """
      foo  bar \{b/@}
          (add a b)
          [1 1]
    """
:-  %noun
foo
