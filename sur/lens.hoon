|%
++  command
  $:  flags/{merge-input/? fan-output/?}
      sources/(list source)
      transformers/(list transformer)
      sinks/(list sink)
  ==
++  source
  $%  {$data data/@}
      {$dojo command/@t}
      {$clay pax/path}
      {$url url/purl}
      {$api api/term command/@t}
      {$get-api api/term endpoint/purl}
      {$listen-api api/term event/term}
  ==
++  transformer
  $%  {$as mar/mark}
      {$hoon code/@t}
  ==
++  sink
  $%  {$stdout $~}
      {$output-file pax/@t}

      {$output-clay pax/path}
      {$url url/purl}
      {$to-api api/term command/@t}
      {$send-api api/term endpoint/purl}
      {$command command/@t}
      {$app app/term}
  ==
--
