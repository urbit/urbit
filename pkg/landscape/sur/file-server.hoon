/-  glob
|%
+$  action
  $%  ::  %serve-dir: from clay directory
      ::
      ::    url-base   site path to route from
      ::    clay-base  clay path to route to
      ::    public     if false, require login
      ::    spa        if true, `404` becomes `clay-base/index.html`
      ::
      [%serve-dir url-base=path clay-base=path public=? spa=?]
      ::  %serve-glob: from glob blobs
      ::
      ::    url-base   site path to route from
      ::    glob       blobs
      ::    public     if false, require login
      ::
      [%serve-glob url-base=path =glob:glob public=?]
      ::  %unserve-dir: remove binding on url-base
      ::
      [%unserve-dir url-base=path]
      ::  %toggle-permission: toggle public flag on url-base
      ::
      [%toggle-permission url-base=path]
      ::  %set-landscape-homepage-prefix: serve landscape at / or /term
      ::
      [%set-landscape-homepage-prefix prefix=(unit term)]
  ==
::
+$  configuration
  $:  landscape-homepage-prefix=(unit term)
  ==
::
+$  update
  $%  [%configuration =configuration]
  ==
--
