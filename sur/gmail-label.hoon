::  This structure is the hoon equivalent of the labels resource used by the
::  gmail api


|%
++  label-list-visibility
  $?  $'labelHide'            ::  Do not show the label in the label list
      $'labelShow'            ::  Show the label in the label list. (Default)
      $'labelShowIfUnread'    ::  Show the label if any unread msgs w/that label.
  ==
++  message-list-visibility
  $?  $hide                    ::  Do not show the label in the message list.
      $show                    ::  Show the label in the message list. (Default)
  ==
--

|%
::  label request is the body of the post request you send to gmail to create
::  a labels resource
++  label-req  {llv/label-list-visibility mlv/message-list-visibility name/@t}

::  the label resource returned by gmail in response to your successful request
++  label  *

++  label-req-to-json  !! 
::  XX belongs in a lib/
::   |=  label-req
::   %-  jobe  :^
::   ['name' `json`s+name]
::   ['labelListVisibility' `json`s+(crip (sifo `cord`llv))]
::   ['messageListVisibility' `json`s+(crip (sifo `cord`mlv))]
::   ~
--
