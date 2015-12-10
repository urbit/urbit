::
::::  /hoon/work/sur
  ::
  ::  A block comment might go here!
  ::
/-    talk
|%
++  client
  _:  tasks+(map @uvH client-task)
      sort+(list @uvH)
  ==
++  client-task
  _:  archived+__(|)
      audience+(set station:talk)
      tax+task
  ==
++  task
  _:  id+@uvH
      date-created+@da
      version+@u
      date-modified+@da
      creator+@p
      doer+(unit @p)
      tags+(set @t)
      date-due+(unit @da)
      done+(unit @da)
      title+@t
      description+@t    ::  XX (list @t)
      discussion+(list comment)
  ==
++  comment
  _:  date+@da
      ship+@p
      body+@t   ::  XX (list @t)
  ==
++  command
  _%  {$new audience+(set station:talk) task}
      {$old id+@uvH dif+update}
      {$sort p+(list @uvH)}
  ==
++  update    ::  XX rename, web-edit?
  _%  _:  $set
  _%  {$audience to+(set station:talk)}
      {$date-due p+(unit @da)}
      {$title p+@t}
      {$description p+@t}
      {$tags p+(set @t)}
      {$done p+?}
  ==  ==
      _:  $add
  _%  {$comment @t}
  ==  ==
      _:  $doer
  _%  {$release $~}
      {$claim $~}
  ==  ==
  ==
--
