::  
::::  /hoon/digitalocean/com/sec
  ::
/+    oauth2
::
::::
  ::
=+  ^=  aut
    %+  oauth2
      'https://app.asana.com/-/oauth_authorize?response_type=code&state=ste'
    'https://app.asana.com/-/oauth_token'
|_  {(bale keys:oauth2) tok/token.aut}
++  aut  ~(. ^aut +<- /read/write)
++  out
  |=  a/hiss
  =;  mow  ~&  db-authorized+mow  mow
  %.  a
  (out-math:aut tok)
++  in   in-code:aut
++  bak  (bak-save-access:aut . |=(tok/token:aut +>(tok tok)))
--

::  create a developer app by logging into your asana account, clicking my
::  profile settings, clicking on 'apps,' and then 'Manage my developer apps' 

::  Be sure to be on https://localhost:8443 and to have registered 
::  'http://localhost:8443/~/ac/asana.com/~./in' as the redirect URI.
::  (If unable to change port number of ship, change the redirect URI port in %eyre)

::  |init-oauth2 |init-oauth2 /com/asana

::  Enter home this sample command to get your user information:
::  +https://app.asana.com/api/1.0/workspaces/{ID}, where 'ID' is the ID of your
::  workspace, which you can easily find by using the Asana API explorer:
::  https://asana.com/developers/api-reference/workspaces

::  Before you receive the response, you'll have to clink on the link.
::  If you successfully auth, you should receive the response in the dojo.


