::  Little app to demonstrate the structure of threads.
::
::  Fetches the top comment of each of the top stories from Hacker News
::
::
/-  spider
/+  *strandio
=,  strand=strand:spider
|%
+$  top-comments  (list tape)
--
=;  core
  ^-  thread:spider
  |=  vase
  =/  m  (strand ,vase)
  ^-  form:m
  ~&  >  'entering main loop'
  ;<  ~  bind:m
    %-  (main-loop ,top-comments)
    :~  handle-print:core
        handle-fetch:core
        ::  `$-(top-comments _*form:(strand ,top-comments))`handle-poll:core
    ==
  (pure:m *vase)
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
::
::  XX
::
++  handle-print
  |=  =top-comments
  =/  m  (strand ,^top-comments)
  ^-  form:m
  ;<  =vase  bind:m  ((handle ,vase) (take-poke %example-fetch-print))
  (print top-comments)
::
::  XX
::
++  print
  |=  =top-comments
  =/  m  (strand ,^top-comments)
  ^-  form:m
  %-  (slog leaf+"drumroll please..." ~)
  ;<  ~  bind:m  (sleep ~s3)
  %-  (slog leaf+"Top comments:" (zing (turn top-comments comment-to-tang)))
  (pure:m top-comments)
::
::  XX
::
++  handle-fetch
  |=  =top-comments
  =/  m  (strand ,^top-comments)
  ^-  form:m
  ;<  =vase  bind:m  ((handle ,vase) (take-poke %example-fetch-fetch))
  (fetch top-comments)
::
::  XX
::
++  fetch
  |=  =top-comments
  =/  m  (strand ,^top-comments)
  ^-  form:m
  =.  top-comments  ~
  %+  (set-timeout ^top-comments)  ~s15
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
  ::  If done, tell subscribers and print the results
  ::
  ?~  top-stories
    (print top-comments)
  ::
  ::  Else, fetch the story info
  ::
  %-  (slog leaf+"fetching item #{+>:(scow %ui i.top-stories)}" ~)
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
::  XX
::
++  handle-poll
  |=  =top-comments
  =/  m  (strand ,^top-comments)
  ^-  form:m
  !!
--
