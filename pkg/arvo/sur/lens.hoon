^?
|%
++  command
  $:  source=source
      sink=sink
  ==
++  source
  $%  [%data data=@]
      [%dojo command=@t]
      [%clay pax=@t]
      [%url url=purl:eyre]
      [%api api=term command=@t]
      [%get-api api=term endpoint=(list @t)]
      [%as mar=mark next=source]          ::  can't be at the
      [%hoon code=@t next=source]         ::  end since they
      [%tuple next=(list source)]         ::  don't bunt well
      [%listen-api api=term event=term]
      [%export app=@t]
      [%import app=@t base64-jam=@t]
      [%export-all ~]
      [%import-all base64-jam=@t]
      [%cancel ~]
  ==
++  sink
  $%  [%stdout ~]
      [%output-file pax=@t]
      [%output-pill pax=@t]
      [%output-clay pax=path]
      [%url url=purl:eyre]
      [%to-api api=term command=@t]
      [%send-api api=term endpoint=(list @t)]
      [%command command=@t]
      [%app app=term]
  ==
--
