::  "Hello world" sample generator  
::
::::  /hoon/hello/gen
  ::
/?    310
::
::::
  ::
!:
:-  %say
|=  *
=>  ^%
    |%
    ++  one
      1
    ++  nine
      1
    ++  two
      (add 1 one)
    ++  three
      (add one two)
    ++  hex  ^#((add hell five))
    ++  hell  65
    ++  hello  65
    ++  hellp  65
    ++  hellq  65
    ++  nov  9
    ++  four
      (add two two)
    ++  five
      +(four)
    --
:-  %noun
hex
