/-    talk
|%
++  client
  $:  tasks=(map ,@uwH client-task)
      sort=(list ,@uwH)
  ==
++  client-task
  $:  task=task
      audience=(set station:talk)
  ==
++  task
  $:  id=@uwH
      date-created=@da
      version=@u
      date-modified=@da
      owner=@p
      status=status
      tags=(set ,@t)
      due-date=(unit ,@da)
      title=@t
      description=@t
      discussion=(list comment)
  ==
++  comment
  $:  date=@da
      ship=@p
      body=@t
  ==
++  status  ?(%took %gave %left)
++  command  
  $%  [%new task]
      [%old id=@uwH dif=update]
      [%sort p=(list ,@uwH)]
  ==
++  update
  $%  $:  %set
  $%  [%due-date p=@da]
      [%title p=@t]
      [%description p=@t]
      [%tags p=(set ,@t)]
      [%done p=(unit ,@da)]
      [%audience p=(set station:talk)]
  ==  ==
      $:  %add
  $%  [%comment [@da @t]]
  ==  ==
      $:  %own
  $%  [%announce ~]
      [%claim ~]
  ==  ==
  ==
--
