::  Little app to demonstrate the structure of programs written with the
::  transaction monad.
::
::  Fetches the top comment of each of the top 10 stories from Hacker News
::
/+  tapp, stdio
::
::  Preamble
::
=>
  |%
  +$  state
    $:  top-comments=(list tape)
    ==
  +$  command  [%noun =cord]
  +$  poke-data
    $%  [%noun cord]
    ==
  +$  out-peer-data
    $%  [%comments (list tape)]
    ==
  +$  in-peer-data  ~
  ++  tapp   (^tapp state command poke-data out-peer-data in-peer-data)
  ++  stdio  (^stdio poke-data out-peer-data)
  --
=>
  |%
  ::  Helper function to print a comment
  ::
  ++  comment-to-tang
    |=  =tape
    ^-  tang
    %+  welp
      %+  turn  (rip 10 (crip tape))
      |=  line=cord
      leaf+(trip line)
    [leaf+""]~
  ::
  ::  All the URLs we fetch from
  ::
  ++  urls
    =/  base  "https://hacker-news.firebaseio.com/v0/"
    :*  top-stories=(weld base "topstories.json")
        item=|=(item=@ud `tape`:(welp base "item/" +>:(scow %ui item) ".json"))
    ==
--
=,  trad=trad:tapp
=,  tapp-trad=tapp-trad:tapp
=,  stdio
::
::  The app
::
%-  create-tapp-poke-peer:tapp
^-  tapp-core-poke-peer:tapp
|_  [=bowl:gall state]
::
::  Main function
::
++  handle-command
  |=  =command
  =/  m  tapp-trad
  ^-  form:m
  ::
  ::  If requested to print, just print what we have in our state
  ::
  ?:  =(cord.command 'print')
    ~&  'drumroll please...'
    ;<  now=@da  bind:m  get-time
    ;<  ~        bind:m  (wait (add now ~s3))
    ~&  'Top comments:'
    %-  (slog (zing (turn top-comments comment-to-tang)))
    (pure:m top-comments)
  ::
  ::  Otherwise, fetch the top HN stories
  ::
  =.  top-comments  ~
  ::
  ::  If this whole thing takes more than 15 seconds, cancel it
  ::
  %+  (set-timeout _top-comments)  (add now.bowl ~s15)
  ;<  =top-stories=json  bind:m  (fetch-json top-stories:urls)
  =/  top-stories=(list @ud)
    ((ar ni):dejs:format top-stories-json)
  ::
  ::  Loop through the first 5 stories
  ::
  =.  top-stories  (scag 5 top-stories)
  |-  ^-  form:m
  =*  loop  $
  ::
  ::  If done, tell subscriers and print the results
  ::
  ?~  top-stories
    ;<  ~  bind:m  (give-result /comments %comments top-comments)
    (handle-command %noun 'print')
  ::
  ::  Else, fetch the story info
  ::
  ~&  "fetching item #{+>:(scow %ui i.top-stories)}"
  ;<  =story-info=json  bind:m  (fetch-json (item:urls i.top-stories))
  =/  story-comments=(unit (list @ud))
    ((ot kids+(ar ni) ~):dejs-soft:format story-info-json)
  ::
  ::  If no comments, say so
  ::
  ?:  |(?=(~ story-comments) ?=(~ u.story-comments))
    =.  top-comments  ["<no top comment>" top-comments]
    loop(top-stories t.top-stories)
  ::
  ::  Else, fetch comment info
  ::
  ;<  =comment-info=json  bind:m  (fetch-json (item:urls i.u.story-comments))
  =/  comment-text=(unit tape)
    ((ot text+sa ~):dejs-soft:format comment-info-json)
  ::
  ::  If no text (eg comment deleted), record that
  ::
  ?~  comment-text
    =.  top-comments  ["<top comment has no text>" top-comments]
    loop(top-stories t.top-stories)
  ::
  ::  Else, add text to state
  ::
  =.  top-comments  [u.comment-text top-comments]
  loop(top-stories t.top-stories)
::
++  handle-peer
  |=  =path
  =/  m  tapp-trad
  ^-  form:m
  ~&  [%baby-take-peer path]
  (pure:m top-comments)
--
