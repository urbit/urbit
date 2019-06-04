::  Creates a 
::
:-  %say
|=  $:  {now/@da eny/@uvJ byk/beak}
        [[ships=(list @p) answers=(list tape) ~] ~]
    ==
:*  %ballot-create
    `(set @p)`(sy ships)
    :*  "Test"
        [[%radio "Question" answers] ~]
    ==
    `@da`(add now ~d1)
==
