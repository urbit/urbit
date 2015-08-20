::
::::  /hoon/talk/sur
  !:
|%
++  audience  (map partner (pair envelope delivery))    ::  destination/state
++  atlas  (map ship status)                            ::  presence map
++  bouquet  (set flavor)                               ::  complete aroma
++  command                                             ::  effect on party
  $%  [%design (pair span (unit config))]               ::  configure/destroy
      [%publish (list thought)]                         ::  originate
      [%review (list thought)]                          ::  deliver
  ==                                                    ::
++  cabal                                               ::  metaconfiguration
  $:  loc=config                                        ::  local config
      ham=(map station config)                          ::  neighborhood configs
  ==
++  config                                              ::  party configuration
  $:  sources=(set partner)                             ::  pulls from
      caption=cord                                      ::  about
      cordon=control                                    ::  restricted to
  ==                                                    ::
++  control  (pair posture (set ship))                  ::  access control
++  delivery                                            ::  delivery state
  $?  %pending                                          ::  undelivered
      %received                                         ::  delivered
      %rejected                                         ::  undeliverable
      %released                                         ::  sent one-way
      %accepted                                         ::  fully processed
  ==                                                    ::
++  envelope  (pair ,? (unit partner))                  ::  visible, sender
++  flavor  path                                        ::  content flavor
++  human                                               ::  human identifier
  $:  true=(unit (trel ,@t (unit ,@t) ,@t))             ::  true name
      hand=(unit ,@t)                                   ::  handle
  ==                                                    ::
++  passport                                            ::  foreign flow
  $%  [%twitter p=@t]                                   ::  twitter
  ==                                                    ::
++  posture                                             ::  security posture
  $?  %black                                            ::  channel
      %white                                            ::  chamber
      %green                                            ::  journal
      %brown                                            ::  mailbox
  ==                                                    ::
++  presence   ?(%gone %hear %talk)                     ::  status type
++  register  (pair atlas (map partner atlas))          ::  ping me, ping srcs
++  shelf  (map span (pair posture cord))               ::  ship shape
++  report                                              ::  talk update
  $%  [%cabal cabal]                                    ::  config neighborhood
  ::  [%folder (list report)]                           ::  multiple
      [%grams (pair ,@ud (list telegram))]              ::  beginning, thoughts
      [%group register]                                 ::  presence
      [%house shelf]                                    ::  station set
  ==                                                    ::
++  speech                                              ::  narrative action
  $%  [%lan p=span q=@t]                                ::  local announce
      [%exp p=@t]                                       ::  hoon line
      [%non ~]                                          ::  no content (yo)
      [%ext p=@tas q=*]                                 ::  extended action
      [%fat p=torso q=speech]                           ::  attachment
      ::  [%inv p=station]                              ::  invite to station
      [%url p=purf]                                     ::  parsed url
      [%ire p=serial q=speech]                          ::  in-reply-to
      [%lin p=? q=@t]                                   ::  no=@, text line
      [%mor p=(list speech)]                            ::  multiplex
      [%app p=@tas q=@t]                                ::  app message
      [%tax p=duty:work-stuff]                          ::
  ==                                                    ::
++  serial     ,@uvH                                    ::  unique identity
++  partner    (each station passport)                  ::  interlocutor
++  status     (pair presence human)                    ::  participant
++  statement  (trel ,@da bouquet speech)               ::  when this
++  station    (pair ship span)                         ::  domestic flow
++  telegram   (pair ship thought)                      ::  who which whom what
++  thought    (trel serial audience statement)         ::  which whom what
++  torso                                               ::  attachment
  $%  [%name (pair ,@t torso)]                          ::  named attachment
      [%text (list ,@t)]                                ::  text lines
      [%tank (list tank)]                               ::  tank list
  ==                                                    ::
++  work-stuff                                          ::
  |%                                                    ::
  ++  duty                                              ::
    $%  [%create p=task]                                ::  create new task
        $:  %update                                     ::  operate on task
            id=@uvH                                     ::  which task
            version=@u                                  ::  version
            $=  meat                                    ::
            $%  [%announce ~]                           ::  make available
                [%release p=ship]                       ::  pass to new owner
                [%accept ~]                             ::  accept pass
                [%delete ~]                             ::  delete task
                [%set-date-due p=(unit ,@da)]           ::  set due date
                [%set-tags p=(set ,@t)]                 ::  set tags
                [%set-title p=@t]                       ::  set title
                [%set-description p=@t]                 ::  set description
                [%set-done p=?]                         ::  set done
                [%add-comment p=@t]                     ::  add comment
            ==                                          ::
        ==                                              ::
    ==                                                  ::
  ++  task                                              ::
    $:  id=@uvH                                         ::
        date-created=@da                                ::
        version=@u                                      ::
        date-modified=@da                               ::
        owner=@p                                        ::
        status=status                                   ::
        tags=(set ,@t)                                  ::
        date-due=(unit ,@da)                            ::
        done=(unit ,@da)                                ::
        title=@t                                        ::
        description=@t                                  ::
        discussion=(list comment)                       ::
    ==                                                  ::
  ++  comment                                           ::
    $:  date=@da                                        ::
        ship=@p                                         ::
        body=@t                                         ::
    ==                                                  ::
  ++  status  ?(%announced %released %accepted)         ::
  --
  ::  markdown
  ::  image
  ::  mime object
--
