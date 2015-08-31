|%
++  twit-do  =>  twit  ,[p=span q=command]              ::  user and action
++  twit-stat  ,[id=@u who=@ta now=@da txt=@t]          ::  recieved tweet
++  twit
  |%
  ++  keys                                              ::  twitter-key type
    $:  con=[tok=@t sec=@t]                             ::  user key pair
        acc=[tok=@t sec=@t]                             ::  app key pair
    ==
  ::
  ++  command                                           ::  poke action
    $%  [%auth p=keys]                                  ::  set API keys
        [%post p=@uvI q=cord]                           ::  post a tweet
    ==
  --
--
