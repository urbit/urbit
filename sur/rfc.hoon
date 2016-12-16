:: This structure is the hoon equivalent of the RFC 822 E-mail message format

|%
++  message  {from/email-address to/email-address subject/@t body/@t}
++  email-address  {name/@t domain/@t}
--
::
|%
++  email-adr-to-text  |=({name/@t domain/@t} (trip (rap 3 name '@' domain ~)))
++  message-to-rfc822
  |=  a/message  ^-  cord
  %-  crip
  """
  From: {(email-adr-to-text from.a)}
  To: {(email-adr-to-text to.a)}
  Subject: {(trip subject.a)}

  {(trip body.a)}
  """
--
