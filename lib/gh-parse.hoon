::  This library includes parsing functions for the json objects
::  that Github's API produces.  In general, the conversion from
::  JSON to urbit types should be performed in marks, so those
::  marks should include this library.
::
/-  gh
=,  format
|%
++  repository
  ^-  $-(json (unit repository:gh))
  =+  dejs-soft
  %-  ot  :~
      'id'^id
      'name'^so
      'full_name'^so
      'owner'^user
      'private'^bo
      'html_url'^so
      'description'^so
      'fork'^bo
      'url'^so
      'forks_url'^so
      'keys_url'^so
      'collaborators_url'^so
      'teams_url'^so
      'hooks_url'^so
      'issue_events_url'^so
      'events_url'^so
      'assignees_url'^so
      'branches_url'^so
      'tags_url'^so
      'blobs_url'^so
      'git_tags_url'^so
      'git_refs_url'^so
      'trees_url'^so
      'statuses_url'^so
      'languages_url'^so
      'stargazers_url'^so
      'contributors_url'^so
      'subscribers_url'^so
      'subscription_url'^so
      'commits_url'^so
      'git_commits_url'^so
      'comments_url'^so
      'issue_comment_url'^so
      'contents_url'^so
      'compare_url'^so
      'merges_url'^so
      'archive_url'^so
      'downloads_url'^so
      'issues_url'^so
      'pulls_url'^so
      'milestones_url'^so
      'notifications_url'^so
      'labels_url'^so
      'releases_url'^so
      'created_at'^so
      'updated_at'^so
      'pushed_at'^so
      'git_url'^so
      'ssh_url'^so
      'clone_url'^so
      'svn_url'^so
      'homepage'^some
      'size'^ni
      'stargazers_count'^ni
      'watchers_count'^ni
      'language'^some
      'has_issues'^bo
      'has_downloads'^bo
      'has_wiki'^bo
      'has_pages'^bo
      'forks_count'^ni
      'mirror_url'^some
      'open_issues_count'^ni
      'forks'^ni
      'open_issues'^ni
      'watchers'^ni
      'default_branch'^so
  ==
++  commit
  ^-  $-(json (unit commit:gh))
  =+  dejs-soft
  %-  ot  :~
      'sha'^so
      'url'^so
      'author'^author
      'committer'^author
      'message'^so
      'tree'^point
      'parents'^(ar point)
      'verification'^verification
  ==
++  user
  ^-  $-(json (unit user:gh))
  =+  dejs-soft
  %-  ot  :~
      'login'^so
      'id'^id
      'avatar_url'^so
      'gravatar_id'^so
      'url'^so
      'html_url'^so
      'followers_url'^so
      'following_url'^so
      'gists_url'^so
      'starred_url'^so
      'subscriptions_url'^so
      'organizations_url'^so
      'repos_url'^so
      'events_url'^so
      'received_events_url'^so
      'type'^so
      'site_admin'^bo
  ==
++  issue
  ^-  $-(json (unit issue:gh))
  |=  jon/json
  =-  (bind - |*(issue/* `issue:gh`[jon issue]))
  %.  jon
  =+  dejs-soft
  %-  ot  :~
      'url'^so
      'labels_url'^so
      'comments_url'^so
      'events_url'^so
      'html_url'^so
      'id'^id
      'number'^ni
      'title'^so
      'user'^user::|+(* (some *user:gh))
      'labels'^(ar label)::|+(* (some *(list label:gh)))::(ar label)
      'state'^so
      'locked'^bo
      'assignee'^(mu user)::|+(* (some *(unit user:gh)))::(mu user)
      'milestone'^some
      'comments'^ni
      'created_at'^so
      'updated_at'^so
      'closed_at'^(mu so)
      'body'^so
  ==
++  author
  ^-  $-(json (unit author:gh))
  =+  dejs-soft
  %-  ot  :~
      'date'^so
      'name'^so
      'email'^so
  ==
++  point
  ^-  $-(json (unit point:gh))
  =+  dejs-soft
  %-  ot  :~
      'url'^so
      'sha'^so
  ==
++  verification
  ^-  $-(json (unit verification:gh))
  =+  dejs-soft
  %-  ot  :~
      'verified'^bo
      'reason'^so
      'signature'^(mu so)
      'payload'^(mu so)
  ==
++  label
  ^-  $-(json (unit label:gh))
  =+  dejs-soft
  %-  ot  :~
      'url'^so
      'name'^so
      'color'^so
  ==
++  comment
  ^-  $-(json (unit comment:gh))
  =+  dejs-soft
  %-  ot  :~
      'url'^so
      'html_url'^so
      'issue_url'^so
      'id'^id
      'user'^user
      'created_at'^so
      'updated_at'^so
      'body'^so
  ==
++  id  no:dejs-soft
++  print-issue
  |=  issue:gh
  =,  format
  ^-  wain
  =+  c=(cury cat 3)
  :*  :(c 'title: ' title ' (#' (rsh 3 2 (scot %ui number)) ')')
      (c 'state: ' state)
      (c 'creator: ' login.user)
      (c 'created-at: ' created-at)
      (c 'assignee: ' ?~(assignee 'none' login.u.assignee))
    ::
      %+  c  'labels: '
      ?~  labels  ''
      |-  ^-  @t
      ?~  t.labels  name.i.labels
      :(c name.i.t.labels ', ' $(t.labels t.t.labels))
    ::
      (c 'comments: ' (rsh 3 2 (scot %ui comments)))
      (c 'url: ' url)
      ''
      %+  turn  (to-wain body)  ::  strip carriage returns
      |=  l/@t
      ?:  =('' l)
        l
      ?.  =('\0d' (rsh 3 (dec (met 3 l)) l))
        l
      (end 3 (dec (met 3 l)) l)
  ==
--
