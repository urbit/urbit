/-    talk
|%
++  client
  $:  tasks=(map ,@uvH client-task)
      sort=(list ,@uvH)
  ==
++  client-task
  $:  task=task
      audience=(set station:talk)
  ==
++  task
  $:  id=@uvH
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
++  status  ?(%announced %released %accepted)
++  command  
  $%  [%new task]
      [%old id=@uvH dif=update]
      [%sort p=(list ,@uvH)]
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
