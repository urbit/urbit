/-    talk
|%
++  client
  $:  tasks=(map ,@uvH client-task)
      sort=(list ,@uvH)
  ==
++  client-task
  $:  archived=_|
      audience=(set station:talk)
      tax=task
  ==
++  task
  $:  id=@uvH
      date-created=@da
      version=@u
      date-modified=@da
      creator=@p
      doer=(unit ,@p)
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
++  command
  $%  [%new audience=(set station:talk) task]
      [%old id=@uvH dif=update]
      [%sort p=(list ,@uvH)]
  ==
++  update
  $%  $:  %set
  $%  [%audience to=(set station:talk)]
      [%date-due p=(unit ,@da)]
      [%title p=@t]
      [%description p=@t]
      [%tags p=(set ,@t)]
      [%done p=?]
  ==  ==
      $:  %add
  $%  [%comment @t]
  ==  ==
      $:  %doer
  $%  [%release ~]
      [%claim ~]
  ==  ==
  ==
--
