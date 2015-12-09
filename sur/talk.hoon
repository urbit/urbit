::
::::  /hoon/talk/sur
  !:
|%
++  audience  (map partner (pair envelope delivery))    ::  destination/state
++  atlas  (map ship status)                            ::  presence map
++  bouquet  (set flavor)                               ::  complete aroma
++  command                                             ::  effect on party
  _%  {$design (pair span (unit config))}               ::  configure/destroy
      {$publish (list thought)}                         ::  originate
      {$review (list thought)}                          ::  deliver
  ==                                                    ::
++  cabal                                               ::  metaconfiguration
  _:  loc+config                                        ::  local config
      ham+(map station config)                          ::  neighborhood configs
  ==                                                    ::
++  config                                              ::  party configuration
  _:  sources+(set partner)                             ::  pulls from
      caption+cord                                      ::  about
      cordon+control                                    ::  restricted to
  ==                                                    ::
++  control  (pair posture (set ship))                  ::  access control
++  delivery                                            ::  delivery state
  _?  $pending                                          ::  undelivered
      $received                                         ::  delivered
      $rejected                                         ::  undeliverable
      $released                                         ::  sent one-way
      $accepted                                         ::  fully processed
  ==                                                    ::
++  envelope  (pair ? (unit partner))                   ::  visible sender
++  flavor  path                                        ::  content flavor
++  human                                               ::  human identifier
  _:  true+(unit (trel @t (unit @t) @t))                ::  true name
      hand+(unit @t)                                    ::  handle
  ==                                                    ::
++  passport                                            ::  foreign flow
  _%  {$twitter p+@t}                                   ::  twitter
  ==                                                    ::
++  posture                                             ::  security posture
  _?  $black                                            ::  channel
      $white                                            ::  chamber
      $green                                            ::  journal
      $brown                                            ::  mailbox
  ==                                                    ::
++  presence   ?($gone $hear $talk)                     ::  status type
++  register  (pair atlas (map partner atlas))          ::  ping me, ping srcs
++  shelf  (map span (pair posture cord))               ::  ship shape
++  report                                              ::  talk update
  _%  {$cabal cabal}                                    ::  config neighborhood
  ::  {$folder (list report)}                           ::  multiple
      {$grams (pair @ud (list telegram))}               ::  beginning thoughts
      {$group register}                                 ::  presence
      {$house shelf}                                    ::  station set
      {$glyph (jug char (set partner))}                 ::  relevant binding
  ==                                                    ::
++  speech                                              ::  narrative action
  _%  {$lan p+span q+@t}                                ::  local announce
      {$exp p+@t}                                       ::  hoon line
      {$non $~}                                         ::  no content (yo)
      {$ext p+@tas q+*}                                 ::  extended action
      {$fat p+torso q+speech}                           ::  attachment
      ::  {$inv p+station}                              ::  invite to station
      {$url p+purf}                                     ::  parsed url
      {$ire p+serial q+speech}                          ::  in-reply-to
      {$lin p+? q+@t}                                   ::  no+@ text line
      {$mor p+(list speech)}                            ::  multiplex
      {$app p+@tas q+@t}                                ::  app message
      {$tax p+duty:work-stuff}                          ::
  ==                                                    ::
++  serial     @uvH                                     ::  unique identity
++  partner    (each station passport)                  ::  interlocutor
++  status     (pair presence human)                    ::  participant
++  statement  (trel @da bouquet speech)                ::  when this
++  station    (pair ship span)                         ::  domestic flow
++  telegram   (pair ship thought)                      ::  who which whom what
++  thought    (trel serial audience statement)         ::  which whom what
++  torso                                               ::  attachment
  _%  {$name (pair @t torso)}                           ::  named attachment
      {$text (list @t)}                                 ::  text lines
      {$tank (list tank)}                               ::  tank list
  ==                                                    ::
++  work-stuff                                          ::
  |%                                                    ::
  ++  duty                                              ::
    _%  {$create tax+task}                              ::  create new task
        {$archive id+@uvH}                              ::  archive task
        {$change id+@uvH meat+flesh}                    ::  request change
        {$update id+@uvH version+@u her+ship meat+flesh}::  broadcast change
    ==                                                  ::
  ++  flesh                                             ::
    _%  {$set-doer her+(unit @p)}                       ::  set doer
        {$set-date-due wen+(unit @da)}                  ::  set due date
        {$set-tags tag+(set @t)}                        ::  set tags
        {$set-title til+@t}                             ::  set title
        {$set-description des+@t} ::  XX (list @t)      ::  set description
        {$set-done don+?}                               ::  set done
        {$add-comment who+@p com+@t}   ::  XX (list @t) ::  add comment
    ==                                                  ::
  ++  task                                              ::
    _:  id+@uvH                                         ::
        date-created+@da                                ::
        version+@u                                      ::
        date-modified+@da                               ::
        creator+@p                                      ::
        doer+(unit @p)                                  ::
        tags+(set @t)                                   ::
        date-due+(unit @da)                             ::
        done+(unit @da)                                 ::
        title+@t                                        ::
        description+@t                                  ::
        discussion+(list comment)                       ::
    ==                                                  ::
  ++  comment                                           ::
    _:  date+@da                                        ::
        ship+@p                                         ::
        body+@t                                         ::
    ==                                                  ::
  --
--
