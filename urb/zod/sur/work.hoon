|%
++  client
  $:  tasks=(map @uwH client-task)
      sort=(list @uwH)
  ==
++  client-task
  $:  task=task
      audience=(set station)
  ==
++  task
  $:  id=@uwH
      date-created=@da
      version=@u
      date-modified=@da
      owner=@p
      status=status
      tags=(set @t)
      due-date=@da
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
--
