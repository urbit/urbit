::  https://developer.github.com/v3/
::
::  These types correspond to the types that Github's API
::  produces, so please check Github's documentation for
::  details.
::
::  For parsing JSON into these types, check out the gh-parse
::  library.
::
|%
++  repository
  $:  id/id
      name/@t
      full-name/@t
      owner/user
      private/?
      html-url/@t
      description/@t
      fork/?
      url/@t
      forks-url/@t
      keys-url/@t
      collaborators-url/@t
      teams-url/@t
      hooks-url/@t
      issue-events-url/@t
      events-url/@t
      assignees-url/@t
      branches-url/@t
      tags-url/@t
      blobs-url/@t
      git-tags-url/@t
      git-refs-url/@t
      trees-url/@t
      statuses-url/@t
      languages-url/@t
      stargazers-url/@t
      contributors-url/@t
      subscribers-url/@t
      subscription-url/@t
      commits-url/@t
      git-commits-url/@t
      comments-url/@t
      issue-comment-url/@t
      contents-url/@t
      compare-url/@t
      merges-url/@t
      archive-url/@t
      downloads-url/@t
      issues-urls/@t
      pulls-url/@t
      milestones-url/@t
      notifications-url/@t
      labels-url/@t
      releases-url/@t
      created-at/time
      updated-at/time
      pushed-at/time
      git-url/@t
      ssh-url/@t
      clone-url/@t
      svn-url/@t
      homepage/json
      size/@ud
      stargazers-count/@ud
      watchers-count/@ud
      language/json
      has-issues/?
      has-downloads/?
      has-wiki/?
      has-pages/?
      forks-count/@ud
      mirror-url/json
      open-issues-count/@ud
      forks/@ud
      open-issues/@ud
      watchers/@ud
      default-branch/@t
  ==
++  commit
  $:  sha/@t
      url/@t
      author/author
      committer/author
      message/@t
      tree/point
      parents/(list point)
      verification/verification
  ==
++  user
  $:  login/@t
      id/id
      avatar-url/@t
      gravatar-id/@t
      url/@t
      html-url/@t
      followers-url/@t
      following-url/@t
      gists-url/@t
      starred-url/@t
      subscriptions-url/@t
      organizations-url/@t
      repos-url/@t
      events-url/@t
      received-events/@t
      type/@t
      site-admin/?
  ==
++  issue
  $:  raw/json
      url/@t
      labels-url/@t
      comments-url/@t
      events-url/@t
      html-url/@t
      id/id
      number/@ud
      title/@t
      user/user
      labels/(list label)
      state/@t
      locked/?
      assignee/(unit user)
      milestone/json
      comments/@ud
      created-at/time
      updated-at/time
      closed-at/(unit time)
      body/@t
  ==
++  author
  $:  date/@t
      name/@t
      email/@t
  ==
++  point
  $:  url/@t
      sha/@t
  ==
++  verification
  $:  verified/?
      reason/@t
      signature/(unit @t)
      payload/(unit @t)
  ==
++  label
  $:  url/@t
      name/@t
      color/@t
  ==
++  comment
  $:  url/@t
      html-url/@t
      issue-url/@t
      id/id
      user/user
      created-at/time
      updated-at/time
      body/@t
  ==
++  id  @t
++  time  @t
++  issues
  $:  repository/repository
      sender/user
      $=  action
      $%  {$assigned assignee/user}
          {$unassigned assignee/user}
          {$labeled label/label}
          {$unlabeled label/label}
          {$opened $~}
          {$closed $~}
          {$reopened $~}
      ==
      issue/issue
  ==
++  issue-comment
  $:  repository/repository
      sender/user
      action/@t
      issue/issue
      comment/comment
  ==
++  ping   {repo/json sender/json hok/(list @t) hook-id/@t zen/json}
--
