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
    =<  $?  de  gr  id  is  la  lo  na  os  pl  qq  sc
            sd  ss  sl  si  st  te  ti  ts  ur  ui  us
        ==
    |%
      ++  de  {$device p/dev}
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
    |*  a/(pair _{term *} (pole _{term *}))  ::  ^-  _{term *}
    ?~  q.a  p.a
    ?(p.a (fork-clams q.a))
  ::
  ++  normalize  ::  XX smarter pretty-printing
    |*  a/_{@ *}  :: ^+  a
    |=  b/*
    ^+  [?@(- . .)]:(a b)
    (a b)
  ::
  ++  doc-data-dry    ::  staticly typed for endpoint lookup
    =,  param
    ^-  (list {typ/_{term (list param)} met/?($get $post) pax/path})
    doc-data
  ::
  ++  doc-data    :: scraped from api docs, used to create types and requests
    ::  ^-  (pole {_{term _(pole *param)} ?($get $post) path})
    =>  param
    :~
      [  {$mentions $~}                %get   /statuses/mentions-timeline  ]
      [  {$posts-by sd $~}             %get   /statuses/user-timeline  ]
      [  {$timeline $~}                %get   /statuses/home-timeline  ]
      [  {$retweets-mine $~}           %get   /statuses/retweets-of-me  ]
      [  {$retweets-of id $~}          %get   /statuses/retweets/':id'  ]
      [  {$show-status id $~}          %get   /statuses/show  ]
      [  {$del-status id $~}           %post  /statuses/destroy/':id'  ]
      [  {$full-status id $~}          %post  /statuses/looup  ]
      [  {$update st $~}               %post  /statuses/update  ]
      [  {$retweet id $~}              %post  /statuses/retweet/':id'  ]
      [  {$unretweet id $~}            %post  /statuses/unretweet/':id'  ]
    ::
      [  {$oembed-from-id id $~}       %get   /statuses/oembed  ]
      [  {$oembed-from-url ur $~}      %get   /statuses/oembed  ]
      [  {$retweeters id $~}           %get   /statuses/retweeters/ids  ]
      [  {$search qq $~}               %get   /search/tweets  ]
      [  {$all-dms $~}                 %get   /direct-messages  ]
      [  {$all-dms-sent $~}            %get   /direct-messages/sent  ]
      [  {$show-dm id $~}              %get   /direct-messages/show  ]
      [  {$del-dm id $~}               %post  /direct-messages/destroy  ]
      [  {$dm sd te $~}                %post  /direct-messages/new  ]
    ::
      [  {$blocked-retweeters $~}      %get   /friendships/no-retweets/ids  ]
      [  {$followers sd $~}            %get   /followers/list  ]
      [  {$follower-ids sd $~}         %get   /followers/ids  ]
      [  {$friends sd $~}              %get   /friends/list  ]
      [  {$friend-ids sd $~}           %get   /friends/ids  ]
      [  {$friend-requests $~}         %get   /friendships/incoming  ]
      [  {$friend-requesting $~}       %get   /friendships/outgoing  ]
      [  {$follow sd $~}               %post  /friendships/create  ]
      [  {$unfollow sd $~}             %post  /friendships/destroy  ]
      [  {$set-friendship sd $~}       %post  /friendships/update  ]
      [  {$relationships ?(us ss) $~}  %get   /friendships/lookup  ]
      :-  {$relationship ?(si os) ?(ti ts) $~}
      [%get /friendships/show]
    ::
      [  {$show-settings $~}           %get   /account/settings  ]
      [  {$test-login $~}              %get   /account/verify-credentials  ]
      [  {$set-settings $~}            %post  /account/settings  ]
      [  {$set-sms-target de $~}       %post  /account/update-delivery-device  ]
      [  {$set-profile $~}             %post  /account/update-profile  ]
      [  {$set-colors $~}              %post  /account/update-profile-colors  ]
      [  {$del-background $~}          %post  /account/remove-profile-banner  ]
      :-  {$set-background $~}
      [%post /account/update-profile-background-image]
    ::
      [  {$blocks $~}                  %get   /blocks/list  ]
      [  {$blocks-ids $~}              %get   /blocks/ids  ]
      [  {$block sd $~}                %post  /blocks/create  ]
      [  {$unblock sd $~}              %post  /blocks/destroy  ]
    ::
      [  {$full-users ?(us ss) $~}     %get   /users/lookup  ]
      [  {$user sd $~}                 %get   /users/show  ]
      [  {$search-users qq $~}         %get   /users/search  ]
      [  {$user-contributees sd $~}    %get   /users/contributees  ] :: undoc'd
      [  {$user-contributors sd $~}    %get   /users/contributors  ] :: undoc'd
      [  {$user-prof sd $~}            %get   /users/profile-banner  ]
    ::
      [  {$mute-user sd $~}            %post  /mutes/users/create  ]
      [  {$unmute-user sd $~}          %post  /mutes/users/destroy  ]
      [  {$muted $~}                   %get   /mutes/users/list  ]
      [  {$muted-ids $~}               %get   /mutes/users/ids  ]
    ::
      [  {$suggested $~}               %get   /users/suggestions  ]
      [  {$suggestion sl $~}           %get   /users/suggestions/':slug'  ]
      :-  {$suggestion-posts sl $~}
      [%get /users/suggestions/':slug'/members]
    ::
      [  {$favorites $~}               %get   /favorites/list  ]
      [  {$del-favorite id $~}         %post  /favorites/destroy  ]
      [  {$favorite id $~}             %post  /favorites/create  ]
    ::
      [  {$lists $~}                   %get   /lists/list  ]
      [  {$lists-of sd $~}             %get   /lists/memberships  ]
      [  {$lists-by sd $~}             %get   /lists/ownerships  ]
      [  {$lists-subscribed sd $~}     %get   /lists/subscriptions  ]
      [  {$list $~}                    %get   /lists/show  ]
      [  {$list-posts $~}              %get   /lists/statuses  ]
      [  {$list-remove ?(us ss) $~}    %post  /lists/members/destroy-all  ]
      [  {$list-subscribers $~}        %get   /lists/subscribers  ]
      [  {$list-subscribe $~}          %post  /lists/subscribers/create  ]
      [  {$list-unsubscribe $~}        %post  /lists/subscribers/destroy  ]
      [  {$list-is-subscribed sd $~}   %get   /lists/subscribers/show  ]
      [  {$list-add ?(us ss) $~}       %post  /lists/members/create-all  ]
      [  {$list-is-in sd $~}           %get   /lists/members/show  ]
      [  {$list-members $~}            %get   /lists/members  ]
      [  {$del-list $~}                %post  /lists/destroy  ]
      [  {$config-list $~}             %post  /lists/update  ]
      [  {$new-list na $~}             %post  /lists/create  ]
    ::
      [  {$saved-searches $~}          %get   /saved-searches/list  ]
      [  {$full-saved-search id $~}    %get   /saved-searches/show/':id'  ]
      [  {$save-search qq $~}          %post  /saved-searches/create  ]
      [  {$del-saved-search id $~}     %post  /saved-searches/destroy/':id'  ]
    ::
      [  {$full-geo id $~}             %get   /geo/id/':id'  ]
      [  {$geo-reverse la lo $~}       %get   /geo/reverse-geocode  ]
      [  {$search-geo $~}              %get   /geo/search  ]
      [  {$geo-similar la lo na $~}    %get   /geo/similar-places  ]
      [  {$trend-locations $~}         %get   /trends/available  ]
      [  {$trends-at id $~}            %get   /trends/place  ]
      [  {$trends-near la lo $~}       %get   /trends/closest  ]
    ::
      [  {$user-report sd $~}          %post  /users/report-spam  ]
      [  {$help-config $~}             %get   /help/configuration  ]
      [  {$help-langs $~}              %get   /help/languages  ]
      [  {$help-privacy $~}            %get   /help/privacy  ]
      [  {$help-tos $~}                %get   /help/tos  ]
      [  {$rate-limit-info $~}         %get   /application/rate-limit-status  ]
    ==
  --
--
