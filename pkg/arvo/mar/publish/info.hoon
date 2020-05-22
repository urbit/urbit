::
::::  /hoon/info/publish/mar
  ::
/-  *publish
!:
|_  info=notebook-info
::
::
++  grow
  |%
  ++  mime
    :-  /text/x-publish-info
    (as-octs:mimes:html (of-wain:format txt))
  ++  txt
    ^-  wain
    :~  (cat 3 'title: ' title.info)
        (cat 3 'description: ' description.info)
        (cat 3 'comments: ' ?:(comments.info 'on' 'off'))
        (cat 3 'writers: ' (spat writers.info))
        (cat 3 'subscribers: ' (spat subscribers.info))
    ==
  --
++  grab
  |%
  ++  mime
    |=  [mite:eyre p=octs:eyre]
    |^  (rash q.p ;~(sfix both-parser (punt gaq)))
    ++  key-val
      |*  [key=@tas val=rule]
      ;~(pfix (jest key) col ace val)
    ++  old-parser
      ;~  (glue gaq)
        (key-val %owner ;~(pfix sig fed:ag))
        (key-val %title (cook crip (star prn)))
        (key-val %filename sym)
        (key-val %comments (perk %open %closed %none ~))
        (key-val %allow-edit (perk %post %comment %all %none ~))
        (key-val %date-created ;~(pfix sig (cook year when:so)))
        (key-val %last-modified ;~(pfix sig (cook year when:so)))
      ==
    ++  new-parser
      ;~  (glue gaq)
        (key-val %title (cook crip (star prn)))
        (key-val %description (cook crip (star prn)))
        (key-val %comments (fuss %on %off))
        (key-val %writers fel:stab)
        (key-val %subscribers fel:stab)
      ==
    ++  both-parser
      ;~  pose
        new-parser
        %+  cook
          |=  [@ title=@t @ comments=@ *]
          ^-  notebook-info
          [title '' =('open' comments) / /]
        old-parser
      ==
    --
  ++  noun  notebook-info
  --
++  grad  %mime
--
