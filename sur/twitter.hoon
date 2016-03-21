|%
++  post  {id/@u who/@ta now/@da txt/@t}              ::  recieved tweet
++  keys                                              ::  twitter-key type
  $:  con/{tok/@t sec/@t}                             ::  user key pair
      acc/{tok/@t sec/@t}                             ::  app key pair
  ==
::
++  command                                           ::  poke action
  $%  {$post p/@uvI q/cord}                           ::  post a tweet
  ==
++  sur-twit  .  :: XX 
::
++  reqs
  |%
  ++  args
    |%
    ++  dev  @t                                       ::  device name
    ++  gat  @t                                       ::  grant type
    ++  lat  @t                                       ::  latitude
    ++  lid  (list tid)                               ::  screen names
    ++  lon  @t                                       ::  longitude
    ++  lsc  (list scr)                               ::
    ++  lst  (list @t)
    ++  nam  @t                                       ::  location name
    ++  pla  @t                                       ::  place-id
    ++  scr  @t                                       ::  screen name
    ++  slu  @t                                       ::  category name
    ++  tid  @u                                       ::  user id
    ++  tok  @t                                       ::  oauth token
    ++  url  @t                                       ::  callback url
    --
  ++  param
    =>  args
    =<  $?  de  fo  gr  id  is  la  lo  na  os  pl  qq  sc
            sd  ss  sl  si  st  te  ti  ts  tr  ur  ui  us
        ==
    |%
      ++  de  {$device p/dev}
      ++  fo  {$follow p/lid}
      ++  gr  {$grant-type p/gat}
      ++  id  {$id p/tid}
      ++  is  {$id p/lid}
      ++  la  {$lat p/lat}
      ++  lo  {$long p/lon}
      ++  na  {$name p/lid}
      ++  os  {$source-screen-name p/scr}
      ++  pl  {$place-id p/pla}
      ++  qq  {$q p/@t}
      ++  sc  {$screen-name p/scr}
      ++  sd  ?(ui sc)
      ++  ss  {$screen-name p/lsc}
      ++  sl  {$slug p/slu}
      ++  si  {$source-id p/tid}
      ++  st  {$status p/@t}
      ++  te  {$text p/@t}
      ++  ti  {$target-id p/tid}
      ++  ts  {$target-screen-name p/scr}
      ++  tr  {$track p/lst}
      ++  ur  {$url p/url}
      ++  ui  {$user-id p/tid}
      ++  us  {$user-id p/lid}
    --
  ::
  ::  the head of every element in ++doc-data is a hoon type for an endpoint
  ::  ++endpoint is the grand union of all of them
  ++  endpoint  (normalize (fork-clams (heads doc-data)))
  ++  heads  |*(a/(pole) ?~(a a [-<.a (heads +.a)]))
  ++  fork-clams
    |*  a/{_{term *} (pole _{term *})}  ::  ^-  _{term *}
    ?~  +.a  -.a
    ?(-.a (fork-clams +.a))
  ::
  ++  normalize  ::  XX smarter pretty-printing
    |*  a/_{@ *}  :: ^+  a
    |=  b/*
    ^+  [?@(- . .)]:(a b)
    (a b)
  ::
  ++  doc-data-dry    ::  staticly typed for endpoint lookup
    ^-  (list {typ/_{term (list param)} met/?($get $post) pax/path}):param
    doc-data
  ::
  ++  doc-data    :: scraped from api docs, used to create types and requests
    ::  ^-  (pole {_{term _(pole *param)} ?($get $post) path})
    =>  param
    :~  
      [  {$stat-ment $~}            %get   /statuses/mentions-timeline  ]
      [  {$stat-user sd $~}         %get   /statuses/user-timeline  ]
      [  {$stat-home $~}            %get   /statuses/home-timeline  ]
      [  {$stat-retw $~}            %get   /statuses/retweets-of-me  ]
      [  {$stat-rets-iddd id $~}    %get   /statuses/retweets/':id'  ]
      [  {$stat-show id $~}         %get   /statuses/show  ]
      [  {$stat-dest-iddd id $~}    %post  /statuses/destroy/':id'  ]
      [  {$stat-upda st $~}         %post  /statuses/update  ]
      [  {$stat-retw-iddd id $~}    %post  /statuses/retweet/':id'  ]
      [  {$stat-oemb-iddd id $~}    %get   /statuses/oembed  ]
      [  {$stat-oemb-urll ur $~}    %get   /statuses/oembed  ]
      [  {$stat-retw-idss id $~}    %get   /statuses/retweeters/ids  ]
      [  {$sear-twee qq $~}         %get   /search/tweets  ]
      :-  {$stat-filt-foll ?(fo tr) $~}
      [%post /statuses/filter]
    ::
      [  {$stat-samp $~}            %get   /statuses/sample  ]
      [  {$stat-fire $~}            %get   /statuses/firehose  ]
      [  {$dire $~}                 %get   /direct-messages  ]
      [  {$dire-sent $~}            %get   /direct-messages/sent  ]
      [  {$dire-show id $~}         %get   /direct-messages/show  ]
      [  {$dire-dest id $~}         %post  /direct-messages/destroy  ]
      [  {$dire-neww sd te $~}      %post  /direct-messages/new  ]
      [  {$frie-nore-idss $~}       %get   /friendships/no-retweets/ids  ]
      [  {$frie-idss sd $~}         %get   /friends/ids  ]
      [  {$foll-idss sd $~}         %get   /followers/ids  ]
      [  {$frie-inco $~}            %get   /friendships/incoming  ]
      [  {$frie-outg $~}            %get   /friendships/outgoing  ]
      [  {$frie-crea sd $~}         %post  /friendships/create  ]
      [  {$frie-dest sd $~}         %post  /friendships/destroy  ]
      [  {$frie-upda sd $~}         %post  /friendships/update  ]
      :-  {$frie-show ?(si os) ?(ti ts) $~}
      [%get /friendships/show]
    ::
      [  {$frie-list sd $~}         %get   /friends/list  ]
      [  {$foll-list sd $~}         %get   /followers/list  ]
      [  {$frie-look ?(us ss) $~}   %get   /friendships/lookup  ]
      [  {$acco-sett-gett $~}       %get   /account/settings  ]
      [  {$acco-veri $~}            %get   /account/verify-credentials  ]
      [  {$acco-sett-post $~}       %post  /account/settings  ]
      [  {$acco-upda-deli de $~}    %post  /account/update-delivery-device  ]
      [  {$acco-upda-prof $~}       %post  /account/update-profile  ]
      :-  {$acco-upda-prof-back $~}
      [%post /account/update-profile-background-image]
    ::
      [  {$acco-upda-prof-colo $~}  %post  /account/update-profile-colors  ]
      [  {$bloc-list $~}            %get   /blocks/list  ]
      [  {$bloc-idss $~}            %get   /blocks/ids  ]
      [  {$bloc-crea sd $~}         %post  /blocks/create  ]
      [  {$bloc-dest sd $~}         %post  /blocks/destroy  ]
      [  {$user-look ?(us ss) $~}   %get   /users/lookup  ]
      [  {$user-show sd $~}         %get   /users/show  ]
      [  {$user-sear qq $~}         %get   /users/search  ]
      [  {$user-cont-tees sd $~}    %get   /users/contributees  ]
      [  {$user-cont-tors sd $~}    %get   /users/contributors  ]
      [  {$acco-remo $~}            %post  /account/remove-profile-banner  ]
      [  {$user-prof sd $~}         %get   /users/profile-banner  ]
      [  {$mute-user-crea sd $~}    %post  /mutes/users/create  ]
      [  {$mute-user-dest sd $~}    %post  /mutes/users/destroy  ]
      [  {$mute-user-idss $~}       %get   /mutes/users/ids  ]
      [  {$mute-user-list $~}       %get   /mutes/users/list  ]
      [  {$user-sugg-slug sl $~}    %get   /users/suggestions  ]
      [  {$user-sugg $~}            %get   /users/suggestions  ]
      [  {$favo-list $~}            %get   /favorites/list  ]
      [  {$favo-dest id $~}         %post  /favorites/destroy  ]
      [  {$favo-crea id $~}         %post  /favorites/create  ]
      [  {$list-list $~}            %get   /lists/list  ]
      [  {$list-stat $~}            %get   /lists/statuses  ]
      [  {$list-memb-dest $~}       %post  /lists/members/destroy  ]
      [  {$list-memb-hips sd $~}    %get   /lists/memberships  ]
      [  {$list-subs-bers $~}       %get   /lists/subscribers  ]
      [  {$list-subs-crea $~}       %post  /lists/subscribers/create  ]
      [  {$list-subs-show sd $~}    %get   /lists/subscribers/show  ]
      [  {$list-subs-dest $~}       %post  /lists/subscribers/destroy  ]
      :-  {$list-memb-crea-alll ?(us ss) $~}
      [%post /lists/members/create-all]
    ::
      [  {$list-memb-show sd $~}    %get   /lists/members/show  ]
      [  {$list-memb-bers $~}       %get   /lists/members  ]
      [  {$list-memb-crea sd $~}    %post  /lists/members/create  ]
      [  {$list-dest $~}            %post  /lists/destroy  ]
      [  {$list-upda $~}            %post  /lists/update  ]
      [  {$list-crea na $~}         %post  /lists/create  ]
      [  {$list-show $~}            %get   /lists/show  ]
      [  {$list-subs-ions sd $~}    %get   /lists/subscriptions  ]
      :-  {$list-memb-dest-alll ?(us ss) $~}
      [%post /lists/members/destroy-all]
    ::
      [  {$list-owne sd $~}         %get   /lists/ownerships  ]
      [  {$save-list $~}            %get   /saved-searches/list  ]
      [  {$save-show-iddd id $~}    %get   /saved-searches/show/':id'  ]
      [  {$save-crea qq $~}         %post  /saved-searches/create  ]
      [  {$save-dest-iddd id $~}    %post  /saved-searches/destroy/':id'  ]
      [  {$geoo-iddd-plac id $~}    %get   /geo/id/':id'  ]
      [  {$geoo-reve la lo $~}      %get   /geo/reverse-geocode  ]
      [  {$geoo-sear $~}            %get   /geo/search  ]
      [  {$geoo-simi la lo na $~}   %get   /geo/similar-places  ]
      [  {$tren-plac id $~}         %get   /trends/place  ]
      [  {$tren-avai $~}            %get   /trends/available  ]
      [  {$tren-clos la lo $~}      %get   /trends/closest  ]
      [  {$user-repo sd $~}         %post  /users/report-spam  ]
      [  {$help-conf $~}            %get   /help/configuration  ]
      [  {$help-lang $~}            %get   /help/languages  ]
      [  {$help-priv $~}            %get   /help/privacy  ]
      [  {$help-toss $~}            %get   /help/tos  ]
      [  {$appl-rate $~}            %get   /application/rate-limit-status  ]
    ==
  --
--
