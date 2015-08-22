/-    talk
|%
++  client
  $:  tasks=(map ,@uvH client-task)
      sort=(list ,@uvH)
  ==
++  client-task
  $:  claiming=_|
      archived=_|
      audience=(set station:talk)
      tax=task
  ==
++  task
  $:  id=@uvH
      date-created=@da
      version=@u
      date-modified=@da
      owner=@p
      status=status
      tags=(set ,@t)
      date-due=(unit ,@da)
      done=(unit ,@da)
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
  $%  [%new audience=(set station:talk) task]
      [%old id=@uvH version=@u dif=update]
      [%sort p=(list ,@uvH)]
      [%audience id=@uvH to=(set station:talk)]
  ==
++  update
  $%  $:  %set
  $%  [%date-due p=(unit ,@da)]
      [%title p=@t]
      [%description p=@t]
      [%tags p=(set ,@t)]
      [%done p=?]
  ==  ==
      $:  %add
  $%  [%comment @t]
  ==  ==
      $:  %own
  $%  [%announce ~]
      [%claim ~]
  ==  ==
  ==
--
