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
  +$  command  cord
  ++  tapp  (^tapp state command)
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
%-  create-tapp:tapp
^-  tapp-core:tapp
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
  ?:  =(command 'print')
    ~&  'drumroll please...'
    ;<  ~  bind:m  (wait (add now.bowl ~s5))
    ~&  'Top comments:'
    %-  (slog (zing (turn top-comments comment-to-tang)))
    (pure:m top-comments)
  ::
  ::  Otherwise, fetch the top HN stories
  ::
  =.  top-comments  ~
  ::
  ::  If this whole thing takes more than 5 seconds, cancel it
  ::
  %+  (set-timeout _top-comments)  (add now.bowl ~s5)
  ;<  =top-stories=json  bind:m  (fetch-json top-stories:urls)
  =/  top-stories=(list @ud)
    ((ar ni):dejs:format top-stories-json)
  ::
  ::  Loop through the first 10 stories
  ::
  =.  top-stories  (scag 10 top-stories)
  |-  ^-  form:m
  =*  loop  $
  ::
  ::  If done, print the results
  ::
  ?~  top-stories
    (handle-command 'print')
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
--
