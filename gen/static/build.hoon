::
::::  /hoon/build/static/gen
  ::
/?  309
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [input=path output=path ~]
        ~
    ==
~_  leaf+"Error: Invalid beam. Try prefixing your path with % or /===/"
:*  %static-action
    %build
    (need (de-beam:format input))
    (need (de-beam:format output))
    %$
==
