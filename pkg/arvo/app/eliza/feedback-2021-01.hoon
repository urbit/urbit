::  feedback-2021-01: questions about engagement for hosting users
::
/+  *eliza
::
%+  build-chain
  'your experience on Urbit so far'
:~  :-  %fun
    [%yesno 'Having fun on urbit yet?']
  ::
    :-  %how-many-groups
    :+  %drift
      (multi:hop ~[- - + +]:[%group-advice %invited])
    :+  %multi  'How many groups are you active in?'
    :~  'None, haven\'t been on much.'
        'Just Urbit Community.'
        'A few!'
        'A lot!!'
    ==
  ::
    :-  %group-advice
    '''
    Joining a group is the best way to use Urbit. Find groups to join in
    Urbit Community (~bitbet-bolbel/urbit-community) in the Bulletin Board
    channel.
    '''
  ::
    :-  %invited
    :+  %drift
      (multi:hop ~[%bugs . .]:%invite-advice)
    :+  %multi  'Try out the invite feature yet?'
    :~  'Yes, already did.'
        'No, but planning to.'
        'No, not planning to.'
    ==
  ::
    :-  %invite-advice
    '''
    To send Urbit invites, log into bridge.urbit.org with your Master
    Ticket, and simply enter a friend's email address. You've got three
    invites to send!
    '''
  ::
    :-  %bugs
    :-  %loose
    'Find any bugs, lacking features, or hiccups in your setup?'
  ::
    :-  %hosting
    :+  %drift
      (yesno:hop %hosting-experience %final-thoughts)
    [%yesno 'Are you on hosting?']
  ::
    :-  %hosting-experience
    :-  %loose
    'Briefly, how has your hosted experience been so far?'
  ::
    :-  %final-thoughts
    :-  %loose
    'Any final thoughts?'
==
