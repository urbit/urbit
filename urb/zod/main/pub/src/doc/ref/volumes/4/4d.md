Dill
====

Dill is our terminal driver.

Data models
-----------

###Arvo

OS events.

####`++gift`, out result

```
++  gift                                                ::  out result <-$
          $%  [%bbye ~]                                 ::  reset prompt
              [%blit p=(list blit)]                     ::  terminal output
              [%init p=@p]                              ::  report install
              [%logo p=@]                               ::  logout
              [%veer p=@ta q=path r=@t]                 ::  install vane
              [%vega p=path]                            ::  reboot by path
              [%verb ~]                                 ::  by %batz
          ==
```

####`++kiss`, in request

```
++  kiss                                                ::  in request ->$
          $%  [%belt p=belt]                            ::  terminal input
              [%blew p=blew]                            ::  terminal config
              [%boot p=*]                               ::  weird %dill boot
              [%crud p=@tas q=(list tank)]              ::  error with trace
              [%flog p=flog]                            ::  wrapped error
              [%hail ~]                                 ::  terminal refresh
              [%hook ~]                                 ::  this term hung up
              [%harm ~]                                 ::  all terms hung up
              [%noop ~]                                 ::  no operation
              [%talk p=tank]                            ::
              [%text p=tape]                            ::
          ==                                            ::
```

```
++  flog                                                ::  sent to %dill
          $%  [%crud p=@tas q=(list tank)]              ::
              [%text p=tape]                            ::
          ==                                            ::
```


####`++note`, out request

```
++  note                                                ::  out request $->
          $%  $:  %b                                    ::  to %batz
          $%  [%hail ~]                                 ::
              [%harm ~]                                 ::
              [%hook ~]                                 ::
              [%kill p=~]                               ::
              [%line p=@t]                              ::
              [%ling ~]                                 ::
              [%make p=(unit ,@t) q=@ud r=@ s=?]        ::
              [%sith p=@p q=@uw r=?]                    ::
          ==  ==                                        ::
              $:  %d                                    ::  to %dill
          $%  [%crud p=@tas q=(list tank)]              ::
              [%text p=tape]                            ::
          ==  ==  ==                                    ::
```

####`++sign`, in result

```
++  sign                                                ::  in result $<-
          $?  $:  %b                                    ::  by %batz
          $%  [%hail ~]                                 ::
              [%helo p=path q=prod]                     ::
              [%logo p=@]                               ::
              [%save p=path q=@]                        ::
              [%sage p=path q=*]                        ::
              [%talk p=tank]                            ::
              [%tell p=(list ,@t)]                      ::
              [%text p=tape]                            ::
              [%verb ~]                                 ::
              [%veer p=@ta q=path r=@t]                 ::
              [%vega p=path]                            ::
              [%warn p=tape]                            ::
          ==  ==                                        ::
              $:  @tas                                  ::  by any
          $%  [%crud p=@tas q=(list tank)]              ::
              [%init p=@p]                              ::
              [%note p=@tD q=tank]                      ::
          ==  ==  ==                                    ::
```
