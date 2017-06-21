::
::::  /hoon/talk/sur
  !:
|%
::
::>  ||
::>  ||  %query-models
::>  ||
::>    models relating to queries, their results and updates.
::+|
::
++  query                                               ::>  query paths
  $%  {$reader $~}                                      ::<  shared ui state
      {$friend $~}                                      ::<  publicly joined
      {$burden who/ship}                                ::<  duties to share
      {$report $~}                                      ::<  duty reports
      {$circle nom/knot ran/range}                      ::<  story query
  ==                                                    ::
++  range  (unit {hed/place tal/(unit place)})          ::<  msg range, @ud/@da
++  place  $%({$da @da} {$ud @ud})                      ::<  point for range
++  prize                                               ::>  query result
  $%  $:  $reader                                       ::<  /reader
          gys/(jug char (set partner))                  ::<  glyph bindings
          nis/(map ship cord)                           ::<  nicknames
      ==                                                ::
      {$friend cis/(set circle)}                        ::<  /friend
      {$burden sos/(map knot burden)}                   ::<  /burden
      {$circle burden}                                  ::<  /circle
  ==                                                    ::
++  rumor                                               ::<  query result change
  $%  $:  $reader                                       ::<  /reader
          $=  dif                                       ::
          $%  {$glyph diff-glyph}                       ::
              {$nick diff-nick}                         ::
          ==                                            ::
      ==                                                ::
      {$friend add/? cir/circle}                        ::<  /friend
      {$burden nom/knot dif/diff-story}                 ::<  /burden
      {$circle dif/diff-story}                          ::<  /circle
  ==                                                    ::
++  burden                                              ::<  full story state
  $:  gaz/(list telegram)                               ::<  all messages
      cos/lobby                                         ::<  loc & rem configs
      pes/crowd                                         ::<  loc & rem presences
  ==                                                    ::
::TODO  deltas into app
++  delta                                               ::
  $%  ::TODO  no more %more, just produce/take list instead!
      {$more mor/(list delta)}                          ::<  multiple changes
      ::  messaging state                               ::
      {$out cir/circle out/(list thought)}              ::<  msgs into outbox
      {$done num/@ud}                                   ::<  msgs delivered
      ::  shared ui state                               ::
      {$glyph diff-glyph}                               ::<  un/bound glyph
      {$nick diff-nick}                                 ::<  changed nickname
      ::  story state                                   ::
      {$story nom/knot dif/diff-story}                  ::<  change to story
      ::  side-effects                                  ::
      {$init $~}                                        ::<  initialize
      {$observe who/ship}                               ::<  watch burden bearer
      {$react ost/bone rac/reaction}  ::TODO  ost.bol?  ::<  reaction to action
      {$quit ost/bone}                                  ::<  force unsubscribe
  ==                                                    ::
++  diff-glyph  {bin/? gyf/char pas/(set partner)}      ::<  un/bound glyph
++  diff-nick   {who/ship nic/cord}                     ::<  changed nickname
++  diff-story                                          ::
  $%  {$new cof/config}                                 ::<  new story
      {$bear bur/burden}                                ::<  new inherited story
      {$burden bur/?}                                   ::<  burden flag
      {$grams gaz/(list telegram)}                      ::<  new/changed msgs
      {$config cir/circle dif/diff-config}              ::<  new/changed config
      {$status pan/partner who/ship dif/diff-status}    ::<  new/changed status
      {$follow sub/? pas/(set partner)}  ::TODO  range  ::<  un/subscribe
      {$remove $~}                                      ::<  removed story
  ==                                                    ::
++  diff-config                                         ::>  config change
  $%  {$full cof/config}                                ::<  set w/o side-effects
      ::TODO  maybe just single partner, since we prob always do that
      {$source add/? pas/(set partner)}                 ::<  add/rem sources
      {$caption cap/cord}                               ::<  changed description
      {$filter fit/filter}                              ::<  changed filter
      {$secure sec/security}                            ::<  changed security
      {$permit add/? sis/(set ship)}                    ::<  add/rem to b/w-list
      {$remove $~}                                      ::<  removed config
  ==                                                    ::
++  diff-status                                         ::>  status change
  $%  {$full sat/status}                                ::<  fully changed status
      {$presence pec/presence}                          ::<  changed presence
      {$human dif/diff-human}                           ::<  changed name
      {$remove $~}                                      ::<  removed config
  ==                                                    ::
++  diff-human                                          ::>  name change
  $%  {$full man/human}                                 ::<  fully changed name
      {$true tru/(unit (trel cord (unit cord) cord))}   ::<  changed true name
      {$handle han/(unit cord)}                         ::<  changed handle
  ==                                                    ::
::
::>  ||
::>  ||  %reader-communication
::>  ||
::>    broker interfaces for readers.
::+|
::
++  action                                              ::>  user action
  $%  ::  circle configuration                          ::
      {$create nom/knot des/cord sec/security}          ::<  create circle
      {$delete nom/knot ano/(unit cord)}                ::<  delete + announce
      {$depict nom/knot des/cord}                       ::<  change description
      {$filter nom/knot fit/filter}                     ::<  change message rules
      {$permit nom/knot inv/? sis/(set ship)}           ::<  invite/banish
      {$source nom/knot sub/? src/(set partner)}        ::<  un/sub to/from src
      ::  messaging                                     ::
      {$convey tos/(list thought)}                      ::<  post exact
      {$phrase aud/(set partner) ses/(list speech)}     ::<  post easy
      ::  personal metadata                             ::
      ::TODO  change to target partners, not only our circles.
      {$status nos/(set knot) sat/status}               ::<  our status update
      ::  changing shared ui                            ::
      {$glyph gyf/char pas/(set partner) bin/?}         ::<  un/bind a glyph
      {$nick who/ship nic/knot}                         ::<  new identity
  ==                                                    ::
++  reaction                                            ::>  user information
  $:  res/?($info $fail)                                ::<  result
      wat/cord                                          ::<  explain
      why/(unit action)                                 ::<  cause
  ==                                                    ::
::
::>  ||
::>  ||  %broker-communication
::>  ||
::>    structures for communicating between brokers.
::+|
::
++  command                                             ::>  effect on story
  $%  {$publish tos/(list thought)}                     ::<  deliver
      {$bearing $~}                                     ::<  prompt to listen
  ==                                                    ::
::
::>  ||
::>  ||  %circles
::>  ||
::>    messaging targets and their metadata.
::+|
::
++  partner    (each circle passport)                   ::<  message target
++  circle     {hos/ship nom/knot}                      ::<  native target
++  passport                                            ::>  foreign target
  $%  {$twitter p/cord}                                 ::<  twitter handle
  ==                                                    ::
::  circle configurations.                              ::
++  lobby      {loc/config rem/(map circle config)}     ::<  our & srcs configs
++  config                                              ::>  circle config
  $:  src/(set partner)                                 ::<  active sources
      cap/cord                                          ::<  description
      fit/filter                                        ::<  message rules
      con/control                                       ::<  restrictions
  ==                                                    ::
++  filter                                              ::>  content filters
  $:  cas/?                                             ::<  dis/allow capitals
      utf/?                                             ::<  dis/allow non-ascii
  ==                                                    ::
++  control    {sec/security ses/(set ship)}            ::<  access control
++  security                                            ::>  security mode
  $?  $black                                            ::<  channel, blacklist
      $white                                            ::<  village, whitelist
      $green                                            ::<  journal, author list
      $brown                                            ::<  mailbox, our r, bl w
  ==                                                    ::
::  participant metadata.                               ::
++  crowd      {loc/group rem/(map partner group)}      ::<  our & srcs presences
++  group      (map ship status)                        ::<  presence map
++  status     {pec/presence man/human}                 ::<  participant
++  presence                                            ::>  status type
  $?  $gone                                             ::<  absent
      $idle                                             ::<  idle
      $hear                                             ::<  present
      $talk                                             ::<  typing
  ==                                                    ::
++  human                                               ::>  human identifier
  $:  tru/(unit (trel cord (unit cord) cord))           ::<  true name
      han/(unit cord)                                   ::<  handle
  ==                                                    ::
::
::>  ||
::>  ||  %message-data
::>  ||
::>    structures for containing main message data.
::+|
::
++  telegram   {aut/ship tot/thought}                   ::<  who thought
++  thought    {uid/serial aud/audience sam/statement}  ::<  which whom this
++  statement  {wen/@da boq/bouquet sep/speech}         ::<  when what body
++  speech                                              ::>  content body
  $%  {$non $~}                                         ::<  no content (yo)
      {$lin pat/? msg/cord}                             ::<  no/@ text line
      {$ire tos/serial sep/speech}                      ::<  in-reply-to
      {$url url/purf}                                   ::<  parsed url
      {$exp exp/cord}                                   ::<  hoon line
      {$fat tac/attache sep/speech}                     ::<  attachment
      {$lan nom/knot msg/cord}                          ::<  local announce
      {$inv inv/? cir/circle}                           ::<  inv/ban for circle
      {$mor ses/(list speech)}                          ::<  multiplex
      {$ext nom/term dat/*}                             ::<  extended action
      {$app app/term msg/cord}                          ::<  app message
      $:  $api                                          ::<  api message
          service/term                                  ::<  service name
          id/cord                                       ::<  id on the service
          id-url/purf                                   ::<  link to id
          summary/cord                                  ::<  summary of event
          body/cord                                     ::<  body of event
          url/purf                                      ::<  link to event
          meta/json                                     ::<  other data for web
      ==                                                ::
  ==                                                    ::
++  attache                                             ::>  attachment
  $%  {$name nom/cord tac/attache}                      ::<  named attachment
      {$text (list cord)}                               ::<  text lines
      {$tank (list tank)}                               ::<  tank list
  ==                                                    ::
::
::>  ||
::>  ||  %message-metadata
::>  ||
::     structures for containing message metadata.
::+|
::
++  serial     @uvH                                     ::<  unique identifier
++  audience   (map partner (pair envelope delivery))   ::<  destination + state
++  envelope   {vis/? sen/(unit partner)}               ::<  visible sender
++  delivery                                            ::>  delivery state
  $?  $pending                                          ::<  undelivered
      $received                                         ::<  delivered
      $rejected                                         ::<  undeliverable
      $released                                         ::<  sent one-way
      $accepted                                         ::<  fully processed
  ==                                                    ::
++  bouquet    (set flavor)                             ::<  complete aroma
++  flavor     path                                     ::<  content flavor
--
