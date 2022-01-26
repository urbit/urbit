/-  *story
/+  lib=story
!:
|_  tale=story
+$  story-additions  [entry-additions=story prose-additions=story]
+$  story-deletions  [entry-deletions=story prose-deletions=story]
+$  story-diff       [additions=story-additions deletions=story-deletions]
::
++  grad
  |%
  ++  form  %story-diff
  ++  diff
    ::  diff between given story, tale and another story, tory
    |=  tory=story
    ^-  story-diff
    ::  Given tory (new story),
    ::        tale (old story),
    ::
    ::  the map of additions is defined as:
    ::    any entries that are present in the new story but not in the old one,
    ::    in conjunction with any additions to the set of proses for existing keys
    ::
    ::  given shared-keys := keys-new AND keys-old 
    ::  entry-additions = (new-story - old-story)  :: entries present in new-story but not in old-story
    ::  prose-additions =  :: for the entries that are present in both, the values only contain proses not found in old-story
    ::    for key in shared-keys, val-new in new-story, val-old in old-story:
    ::       [key (val-new - val-old)]
    ::  additions = entry-additions UNION prose-additions
    ::
    ::  deletions is the same thing, but with new and old swapped.
    ::  (so anything not in the new but still in the old)
    ::
    =/  entry-additions  (~(dif by tory) tale)  :: tory(new) - tale(old)
    =/  entry-deletions  (~(dif by tale) tory)  :: tale(old) - tory(new)
    ::
    ::  a map with only those kv-pairs that are in both tory and tale.
    ::  used to filter out any entries that were not in one or the other while calculating the set differences
    ::  we keep it as a map so that it can be used in int by
    ::  but we discard all of its values because we overwrite them in the next step
    ::
    =/  entries-both  (~(int by tale) tory)
    ::
    ::  A - all entries in (tory(new) - tale(old)) + all entries in (tale(old) - tory(new)), but,
    ::      if the key of an entry is present in both, the final value of that entry will be (val:tory(new) - val:tale(tale))
    ::  B - only those entries in A whose keys are present in both tory(new) and tale(old) 
    ::
    =/  prose-additions
      %-  ~(int by entries-both)  :: B
      %-  (~(uno by tory) tale)   :: A
      |=  [k=tako:clay proses-tory=(set prose) proses-tale=(set prose)]
      ^-  (set prose)
      (~(dif in proses-tory) proses-tale)
    ::
    =/  prose-deletions
      %-  ~(int by entries-both)
      %-  (~(uno by tale) tory)
      |=  [k=tako:clay proses-tale=(set prose) proses-tory=(set prose)]
      ^-  (set prose)
      (~(dif in proses-tale) proses-tory)
    ::
    ::  invariant (~(int by entry-additions) prose-additions) = ~
    ::  and vice versa for deletions 
    ::  =/  additions=story  (~(uni by entry-additions) prose-additions)
    ::  =/  deletions=story  (~(uni by entry-deletions) prose-deletions)
    [[entry-additions prose-additions] [entry-deletions prose-deletions]]
  ++  pact
    ::  given tale=story, compute the new story after applying the given dif
    |=  dif=story-diff
    ^-  story
    ::  XX code clean up: better way to declare vars
    =/  [[entry-additions=story prose-additions=story] [entry-deletions=story prose-deletions=story]]  dif
    =.  tale  (~(uni by tale) entry-additions)  :: add completely new entries
    =.  tale                                    :: add new proses to existing entries
      %-  (~(uno by tale) prose-additions)
      |=  [k=tako:clay proses-tale=(set prose) proses-additions=(set prose)]
      ^-  (set prose)
      (~(uni in proses-tale) proses-additions)
    ::
    =.  tale  (~(dif by tale) entry-deletions)  :: remove any entries which should not be present
    =.  tale                                    :: remove any proses from existing entries
      %-  ~(urn by tale)
      |=  [k=tako:clay proses=(set prose)]
      ^-  (set prose)
      ::  This works because get:ju returns the empty set if the key isn't found
      =/  proses-del=(set prose)  (~(get ju prose-deletions) k)
      (~(dif in proses) proses-del)
    ::
    tale
  ++  join
    |=  [ali=story-diff bob=story-diff]
    ^-  (unit story-diff)
    =/  joined-entry-adds  (~(uni by entry-additions.additions.ali) entry-additions.additions.bob)
    =/  joined-proses-adds
      %-  (~(uno by prose-additions.additions.ali) prose-additions.additions.bob)
      |=  [k=tako:clay proses-a=(set prose) proses-b=(set prose)]
      ^-  (set prose)
      (~(uni in proses-a) proses-b)
    ::
    =/  joined-entry-dels  (~(uni by entry-deletions.deletions.ali) entry-deletions.deletions.bob)
    =/  joined-deletion-dels
      %-  (~(uno by prose-deletions.deletions.ali) prose-deletions.deletions.bob)
      |=  [k=tako:clay proses-a=(set prose) proses-b=(set prose)]
      ^-  (set prose)
      (~(uni in proses-a) proses-b)
    :: in a true join, we'd do an intersection and see if the vals are not exactly the same
    :: which means we have a conflict, then we'd produce null, kick the flow to mash
    %-  some
    [[joined-entry-adds joined-proses-adds] [joined-entry-dels joined-deletion-dels]]
  ++  mash
    ::  called by meld, force merge, annotating conflicts
    |=  $:  [als=ship ald=desk ali=story-diff]
            [bos=ship bod=desk bob=story-diff]
        ==
    ^-  story-diff
    (need (join ali bob)) :: XX temporary, only because join doesn't fail
  --
::
++  grow                                                ::  convert to
  |%                                                    ::
  ++  mime                                              ::  to %mime
    [/text/x-urb-story (as-octs:mimes:html (of-wain:format txt))]
  ++  txt
    ^-  wain
    ::  XX code clean up: cleaner snoc
    %+  snoc  :: ensures terminating newline is present
    %+  murn  ~(tap by tale)
    |=  [[=tako:clay proses=(set prose)]]
    ^-  (unit cord)
    ?~  proses  ~
    %-  some
    %-  crip
    ;:  welp
      (tako-to-text:lib tako)
      (trip (proses-to-text:lib proses))
      "---"
    ==
    ''
  --
++  grab
  |%                                             ::  convert from
  ++  noun  story                                ::  clam from %noun
  ++  mime                                       ::  retrieve form %mime
    |=  [p=mite q=octs]
    ^-  story
    =/  commit-parser 
      ;~  sfix                                     :: throw away the trailing newline
        ;~  pfix  (jest 'commit: ')                :: throw away leading literal 'commit'
          (cook @uv ;~(pfix (jest '0v') viz:ag))   :: parse a @uv
        ==
      ::
        (just '\0a')                               :: parse trailing newline
      ==
    ::
    =/  title-parser
      ;~  sfix                  :: throw away trailing newlines
        (cook crip (star prn))  :: parse any number of ascii characters, turn into cord
        (jest '\0a\0a')         :: parse two newlines
      ==
    ::
    =/  body-parser
      ;~  sfix                              :: parse the following and discard terminator
        %-  star                            :: parse 0 or more of the following
        %+  cook  crip                      :: convert to cord
        ;~  less  (jest '---\0a')           :: exclude the terminator from the following parse
          ;~(sfix (star prn) (just '\0a'))  :: parse 0 or more prn chars then discard literal newline
        ==
      ::
        (jest '---\0a')                     :: parse the terminator
      ==
    ::
    =/  story-parser
      %-  star           :: parse any number of the chapters
      ;~  plug           :: parse chapter: a commit, followed by a title, followed by a body
          commit-parser
          title-parser
          body-parser
      ==
    ::
    =/  story-text    `@t`q.q
    =/  parsed-story  `(list [@uv @t wain])`(rash story-text story-parser)
    %-  ~(gas by *story)
    %+  turn  parsed-story
    |=  [=tako:clay title=@t body=wain]
    :-  tako 
    %-  silt
    :~  [title (of-wain:format body)]
    ==
  --
--
