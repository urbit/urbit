::    A Twitter API library.
::
::::  /hoon/twitter/lib
  ::
/?    314
/-    twitter
/+    oauth1
=+  sur-twit:^twitter  :: XX
!:
::::  functions
  ::
|%
++  fass                                                ::  rewrite path
  |=  a/path
  %+  turn  a
  |=(b/@t (gsub '-' '_' b))
::
++  gsub                                                ::  replace chars
  |=  {a/@t b/@t t/@t}
  ^-  @t
  ?:  =('' t)  t
  %+  mix  (lsh 3 1 $(t (rsh 3 1 t)))
  =+  c=(end 3 1 t)
  ?:(=(a c) b c)
::
++  interpolate-some  ::  [/a/:b/c [d+'bar' b+'foo']~] -> [/a/foo/c [d+'bar']~]
  |=  {pax/path quy/quay}  ^-  {path quay}
  =+  ^=  inline                                        ::  required names
      %-  ~(gas in *(set term))
      (murn pax replacable:interpolate-path)
  =^  inter  quy
    (skid quy |=({a/knot @} (~(has in inline) a)))
  [(interpolate-path pax inter) quy]
::
++  interpolate-path  ::  [/a/:b/c [%b 'foo']~] -> /a/foo/c
  =+  replacable=|=(a/knot `(unit term)`(rush a ;~(pfix col sym)))
  |=  {a/path b/(list (pair term knot))}  ^-  path
  ?~  a  ?~(b ~ ~|(unused-values+b !!))
  =+  (replacable i.a)
  ?~  -  [i.a $(a t.a)]  ::  literal value
  ?~  b  ~|(no-value+u !!)
  ?.  =(u p.i.b)  ~|(mismatch+[u p.i.b] !!)
  [q.i.b $(a t.a, b t.b)]
::
++  valve                                               ::  produce request
  |=  $:  med/?($get $post)
          {pax/path quy/quay}
          key/keys
          est/time 
          eny/@
      ==
  ^-  hiss
  =+  url=(scan "https://api.twitter.com/1.1.json" auri:epur)  :: base path
  =.  q.q.url  (welp q.q.url pax)
  =+  hed=(~(add ja *math) %authorization (oauth1 med url quy key est eny))
  ?-  med
    $get  [url(r quy) med hed ~]
    $post
      =.  hed  (~(add ja hed) %content-type 'application/x-www-form-urlencoded')
      [url med hed (some (tact +:(tail:earn quy)))]
  ==
--
!:
::::  library
  ::
|%
++  twip                                                ::  response printers
  |%
  ++  mean
    |=  {msg/@t num/@ud}  ^-  tank
    rose+[": " `~]^~[leaf+"Error {<num>}" leaf+(trip msg)]
  --
++  twir                                                ::  response parsers
  |%
  ++  fasp  |*(a/{@tas *} [(gsub '-' '_' -.a) +.a])     ::  XX usable electroplating
  ++  user  (cook crip (plus ;~(pose aln cab)))
  ++  mean  (ot errors+(ar (ot message+so code+ni ~)) ~):jo
  ++  stat
    =+  jo
    ^-  $-(json (unit {id+@u who+@ta now+@da txt+@t}))
    %-  ot
    :~  id+ni
        user+(ot (fasp screen-name+(su user)) ~)
        (fasp created-at+da)
        text+so
    ==
  ++  usel 
    =+  jo
    ^-  $-(json (unit (list who/@ta)))
    =-  (ot users+(ar -) ~)
    (ot (fasp screen-name+(su user)) ~)
  --
++  main
  =>  |%                                                ::  request structures
      ++  dev  @t                                       ::  device name
      ++  gat  @t                                       ::  grant type
      ++  lat  @t                                       ::  latitude
      ++  lid  (list tid)
      ++  lon  @t                                       ::  longitude
      ++  lsc  (list scr)
      ++  lst  (list @t)
      ++  nam  @t                                       ::  location name
      ++  pla  @t                                       ::  place-id
      ++  scr  @t                                       ::  screen name
      ++  slu  @t                                       ::  category name
      ++  tid  @u 
      ++  tok  @t                                       ::  oauth token
      ++  url  @t                                       ::  callback url
      ::
      ++  at  {$access-token p/tok}
      ++  de  {$device p/dev}
      ++  fo  {$follow p/lid}
      ++  gr  {$grant-type p/gat}
      ++  id  {$id p/tid}
      ++  ii  {$'!inline' p/@t}
      ++  is  {$id p/lid}
      ++  la  {$lat p/lat}
      ++  lo  {$long p/lon}
      ++  na  {$name p/lid}
      ++  oa  {$oauth-callback p/url}
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
  |_  {key/keys est/time eny/@uv}
  ++  lutt  |=(@ `@t`(rsh 3 2 (scot %ui +<)))
  ++  llsc  
    |=  (list scr) 
    (roll +< |=({p/scr q/@t} (cat 3 (cat 3 q ',') p)))
  ::
  ++  llst  
    |=  (list @t) 
    (roll +< |=({p/@t q/@t} (cat 3 (cat 3 q ',') p)))
  ::
  ++  llid
    |=  (list tid) 
    (roll +< |=({p/tid q/@t} (cat 3 (cat 3 q ',') (lutt p))))
  ::
  ++  mold                                        ::  construct request
    |*  {med/meth pax/path a/$-(* *)}
    |=  {args/a quy/quay}
    (valve med (cowl pax args quy) key est eny)
  ::
  ++  cowl                                        ::  handle parameters
    |=  $:  pax/path 
            ban/(list {p/@t q/?(@ (list @))})
            quy/quay
        ==
    ^-  {tape:path quay}
    ?~  ban
      [(fass pax) quy]
    ?:  =('!inline' p.i.ban)
      ?@  q.i.ban
        [(fass (welp pax /[`@t`q.i.ban])) quy]
      !!
    :-  (fass pax)
    %+  welp  quy
    %+  turn  `(list {p/@t q/?(@ (list @))})`ban
    |=  {p/@t q/?(@ (list @))}
    ^-  {@t @t}
    :-  (gsub '-' '_' p)
    ?@  q
      ?-  p
        ?($id $source-id $target-id $user-id)  (lutt q)
        @                                      `@t`q
      ==
    ?-  p
      ?($follow $id $name $user-id)  (llid q)
      $track                         (llst q)
      $screen-name                   (llsc q)
      *                              !!
    ==
  ::
  ++  stat-ment  
    (mold %get /statuses/mentions-timeline $~)
  ::
  ++  stat-user  
    (mold %get /statuses/user-timeline {sd $~})
  ::
  ++  stat-home  
    (mold %get /statuses/home-timeline $~)
  ::
  ++  stat-retw  
    (mold %get /statuses/retweets-of-me $~)
  ::
  ++  stat-rets-iddd  
    (mold %get /statuses/retweets {ii $~})
  ::
  ++  stat-show  
    (mold %get /statuses/show {id $~})
  ::
  ++  stat-dest-iddd  
    (mold %post /statuses/destroy {ii $~})
  ::
  ++  stat-upda  
    (mold %post /statuses/update {st $~})
  ::
  ++  stat-retw-iddd  
    (mold %post /statuses/retweet {ii $~})
  ::
  ++  stat-oemb-iddd  
    (mold %get /statuses/oembed {id $~})
  ::
  ++  stat-oemb-urll  
    (mold %get /statuses/oembed {ur $~})
  ::
  ++  stat-retw-idss  
    (mold %get /statuses/retweeters/ids {id $~})
  ::
  ++  sear-twee  
    (mold %get /search/tweets {qq $~})
  ::
  ++  stat-filt-foll  
    (mold %post /statuses/filter {?(fo tr) $~})
  ::
  ++  stat-samp  
    (mold %get /statuses/sample $~)
  ::
  ++  stat-fire  
    (mold %get /statuses/firehose $~)
  ::
  ++  user  
    (mold %get /user $~)
  ::
  ++  site  
    (mold %get /site {fo $~})
  ::
  ++  dire  
    (mold %get /direct-messages $~)
  ::
  ++  dire-sent  
    (mold %get /direct-messages/sent $~)
  ::
  ++  dire-show  
    (mold %get /direct-messages/show {id $~})
  ::
  ++  dire-dest  
    (mold %post /direct-messages/destroy {id $~})
  ::
  ++  dire-neww  
    (mold %post /direct-messages/new {sd te $~})
  ::
  ++  frie-nore-idss  
    (mold %get /friendships/no-retweets/ids $~)
  ::
  ++  frie-idss  
    (mold %get /friends/ids {sd $~})
  ::
  ++  foll-idss  
    (mold %get /followers/ids {sd $~})
  ::
  ++  frie-inco  
    (mold %get /friendships/incoming $~)
  ::
  ++  frie-outg  
    (mold %get /friendships/outgoing $~)
  ::
  ++  frie-crea  
    (mold %post /friendships/create {sd $~})
  ::
  ++  frie-dest  
    (mold %post /friendships/destroy {sd $~})
  ::
  ++  frie-upda  
    (mold %post /friendships/update {sd $~})
  ::
  ++  frie-show  
    (mold %get /friendships/show {?(si os) ?(ti ts) $~})
  ::
  ++  frie-list  
    (mold %get /friends/list {sd $~})
  ::
  ++  foll-list  
    (mold %get /followers/list {sd $~})
  ::
  ++  frie-look  
    (mold %get /friendships/lookup {?(us ss) $~})
  ::
  ++  acco-sett-gett  
    (mold %get /account/settings $~)
  ::
  ++  acco-veri  
    (mold %get /account/verify-credentials $~)
  ::
  ++  acco-sett-post  
    (mold %post /account/settings $~)
  ::
  ++  acco-upda-deli  
    (mold %post /account/update-delivery-device {de $~})
  ::
  ++  acco-upda-prof  
    (mold %post /account/update-profile $~)
  ::
  ++  acco-upda-prof-back  
    (mold %post /account/update-profile-background-image $~)
  ::
  ++  acco-upda-prof-colo  
    (mold %post /account/update-profile-colors $~)
  ::
  ++  bloc-list  
    (mold %get /blocks/list $~)
  ::
  ++  bloc-idss  
    (mold %get /blocks/ids $~)
  ::
  ++  bloc-crea  
    (mold %post /blocks/create {sd $~})
  ::
  ++  bloc-dest  
    (mold %post /blocks/destroy {sd $~})
  ::
  ++  user-look  
    (mold %get /users/lookup {?(us ss) $~})
  ::
  ++  user-show  
    (mold %get /users/show {sd $~})
  ::
  ++  user-sear  
    (mold %get /users/search {qq $~})
  ::
  ++  user-cont-tees  
    (mold %get /users/contributees {sd $~})
  ::
  ++  user-cont-tors  
    (mold %get /users/contributors {sd $~})
  ::
  ++  acco-remo  
    (mold %post /account/remove-profile-banner $~)
  ::
  ++  user-prof  
    (mold %get /users/profile-banner {sd $~})
  ::
  ++  mute-user-crea  
    (mold %post /mutes/users/create {sd $~})
  ::
  ++  mute-user-dest  
    (mold %post /mutes/users/destroy {sd $~})
  ::
  ++  mute-user-idss  
    (mold %get /mutes/users/ids $~)
  ::
  ++  mute-user-list  
    (mold %get /mutes/users/list $~)
  ::
  ++  user-sugg-slug  
    (mold %get /users/suggestions {sl $~})
  ::
  ++  user-sugg  
    (mold %get /users/suggestions $~)
  ::
  ++  favo-list  
    (mold %get /favorites/list $~)
  ::
  ++  favo-dest  
    (mold %post /favorites/destroy {id $~})
  ::
  ++  favo-crea  
    (mold %post /favorites/create {id $~})
  ::
  ++  list-list  
    (mold %get /lists/list $~)
  ::
  ++  list-stat  
    (mold %get /lists/statuses $~)
  ::
  ++  list-memb-dest  
    (mold %post /lists/members/destroy $~)
  ::
  ++  list-memb-hips  
    (mold %get /lists/memberships {sd $~})
  ::
  ++  list-subs-bers  
    (mold %get /lists/subscribers $~)
  ::
  ++  list-subs-crea  
    (mold %post /lists/subscribers/create $~)
  ::
  ++  list-subs-show  
    (mold %get /lists/subscribers/show {sd $~})
  ::
  ++  list-subs-dest  
    (mold %post /lists/subscribers/destroy $~)
  ::
  ++  list-memb-crea-alll  
    (mold %post /lists/members/create-all {?(us ss) $~})
  ::
  ++  list-memb-show  
    (mold %get /lists/members/show {sd $~})
  ::
  ++  list-memb-bers  
    (mold %get /lists/members $~)
  ::
  ++  list-memb-crea  
    (mold %post /lists/members/create {sd $~})
  ::
  ++  list-dest  
    (mold %post /lists/destroy $~)
  ::
  ++  list-upda  
    (mold %post /lists/update $~)
  ::
  ++  list-crea  
    (mold %post /lists/create {na $~})
  ::
  ++  list-show  
    (mold %get /lists/show $~)
  ::
  ++  list-subs-ions  
    (mold %get /lists/subscriptions {sd $~})
  ::
  ++  list-memb-dest-alll  
    (mold %post /lists/members/destroy-all {?(us ss) $~})
  ::
  ++  list-owne  
    (mold %get /lists/ownerships {sd $~})
  ::
  ++  save-list  
    (mold %get /saved-searches/list $~)
  ::
  ++  save-show-iddd  
    (mold %get /saved-searches/show {ii $~})
  ::
  ++  save-crea  
    (mold %post /saved-searches/create {qq $~})
  ::
  ++  save-dest-iddd  
    (mold %post /saved-searches/destroy {ii $~})
  ::
  ++  geoo-iddd-plac  
    (mold %get /geo/id {ii $~})
  ::
  ++  geoo-reve  
    (mold %get /geo/reverse-geocode {la lo $~})
  ::
  ++  geoo-sear  
    (mold %get /geo/search $~)
  ::
  ++  geoo-simi  
    (mold %get /geo/similar-places {la lo na $~})
  ::
  ++  tren-plac  
    (mold %get /trends/place {id $~})
  ::
  ++  tren-avai  
    (mold %get /trends/available $~)
  ::
  ++  tren-clos  
    (mold %get /trends/closest {la lo $~})
  ::
  ++  user-repo  
    (mold %post /users/report-spam {sd $~})
  ::
  ++  oaut-auth-cate  
    (mold %get /oauth/authenticate $~)
  ::
  ++  oaut-auth-rize  
    (mold %get /oauth/authorize $~)
  ::
  ++  oaut-acce  
    (mold %post /oauth/access-token $~)
  ::
  ++  oaut-requ  
    (mold %post /oauth/request-token {oa $~})
  ::
  ++  oaut-toke  
    (mold %post /oauth2/token {gr $~})
  ::
  ++  oaut-inva  
    (mold %post /oauth2/invalidate-token {at $~})
  ::
  ++  help-conf  
    (mold %get /help/configuration $~)
  ::
  ++  help-lang  
    (mold %get /help/languages $~)
  ::
  ++  help-priv  
    (mold %get /help/privacy $~)
  ::
  ++  help-toss  
    (mold %get /help/tos $~)
  ::
  ++  appl-rate  
    (mold %get /application/rate-limit-status $~)
  ::
  ++  stat-look  
    (mold %get /statuses/lookup {us $~})
  --
--
