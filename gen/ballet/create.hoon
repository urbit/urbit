::  Creates a 
::

=/  people=(list @p)
  [~littel-ponnys ~rovnys-ricfer ~palfun-foslup ~rapfyr-diglyt ~poldec-tonteg ~]
::
:-  %say
|=  $:  {now/@da eny/@uvJ byk/beak}
        [[answers=(list tape) ~] ~]
    ==
:*  %ballot-create
    `(set @p)`(sy people)
    :*  "Test"
        [[%radio "Question" answers] ~]
    ==
    `@da`(add now ~d1)
==
