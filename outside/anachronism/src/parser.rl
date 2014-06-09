#include <string.h>
#include <anachronism/parser.h>

#define BASE_EV(ev, t) \
  (ev).SUPER_.type = TELNET_EV_PARSER_##t

#define EV_DATA(ev, text, len) do {\
  BASE_EV(ev, DATA);\
  (ev).data = (text);\
  (ev).length = (len);\
} while (0)

#define EV_COMMAND(ev, cmd) do {\
  BASE_EV(ev, COMMAND);\
  (ev).command = (cmd);\
} while (0)

#define EV_OPTION(ev, cmd, opt) do {\
  BASE_EV(ev, OPTION);\
  (ev).command = (cmd);\
  (ev).option = (opt);\
} while (0)

#define EV_SUBNEGOTIATION(ev, act, opt) do {\
  BASE_EV(ev, SUBNEGOTIATION);\
  (ev).active = (act);\
  (ev).option = (opt);\
} while (0)

#define EV_WARNING(ev, msg, pos) do {\
  BASE_EV(ev, WARNING);\
  (ev).message = (msg);\
  (ev).position = (pos);\
} while (0)

struct telnet_parser {
  int cs; /* current Ragel state */
  const telnet_byte* p; /* current position */
  const telnet_byte* pe; /* end of current packet */
  const telnet_byte* eof; /* end-of-file marker */
  
  telnet_byte option_mark; /* temporary storage for a command byte */
  unsigned char interrupted; /* Flag for interrupts */
  
  telnet_parser_callback callback; /* Receiver of Telnet events*/
  void* userdata; /* Context for parser callback */
};

%%{
  machine telnet_parser;
  
  access parser->;
  variable p parser->p;
  variable pe parser->pe;
  variable eof parser->eof;
  
  action flush_text {
    if (parser->callback && buflen > 0)
    {
      telnet_parser_data_event ev;
      EV_DATA(ev, buf, buflen);
      parser->callback(parser, (telnet_parser_event*)&ev);
      buflen = 0;
    }
  }
  
  action char {
    if (parser->callback && buf)
      buf[buflen++] = fc;
  }
  
  action basic_command {
    if (parser->callback && buf)
    {
      telnet_parser_command_event ev;
      EV_COMMAND(ev, fc);
      parser->callback(parser, (telnet_parser_event*)&ev);
    }
  }

  action option_mark {
    parser->option_mark = fc;
  }
  action option_command {
    if (parser->callback && buf)
    {
      telnet_parser_option_event ev;
      EV_OPTION(ev, parser->option_mark, fc);
      parser->callback(parser, (telnet_parser_event*)&ev);
    }
  }

  action subneg_command {
    parser->option_mark = fc;
    if (parser->callback && buf != NULL)
    {
      telnet_parser_subnegotiation_event ev;
      EV_SUBNEGOTIATION(ev, 1, parser->option_mark);
      parser->callback(parser, (telnet_parser_event*)&ev);
    }
  }
  action subneg_command_end {
    if (parser->callback && buf != NULL)
    {
      telnet_parser_subnegotiation_event ev;
      EV_SUBNEGOTIATION(ev, 0, parser->option_mark);
      parser->callback(parser, (telnet_parser_event*)&ev);
    }
  }

  action warning_cr {
    if (parser->callback && buf != NULL)
    {
      telnet_parser_warning_event ev;
      EV_WARNING(ev, "Invalid \\r: not followed by \\n or \\0.", fpc-data);
      parser->callback(parser, (telnet_parser_event*)&ev);
    }
  }
  action warning_iac {
    if (parser->callback && buf != NULL)
    {
      telnet_parser_warning_event ev;
      EV_WARNING(ev, "IAC followed by invalid command.", fpc-data);
      parser->callback(parser, (telnet_parser_event*)&ev);
    }
  }
  
  include telnet_parser_common "parser_common.rl";
  write data;
}%%

telnet_parser* telnet_parser_new(void* userdata,
                                 telnet_parser_callback callback)
{
  telnet_parser* parser = malloc(sizeof(telnet_parser));
  if (parser)
  {
    memset(parser, 0, sizeof(*parser));
    %% write init;
    parser->callback = callback;
    parser->userdata = userdata;
  }
  return parser;
}

void telnet_parser_free(telnet_parser* parser)
{
  free(parser);
}

telnet_error telnet_parser_get_userdata(telnet_parser* parser, void** userdata)
{
  if (!parser)
    return TELNET_E_BAD_PARSER;
  
  *userdata = parser->userdata;
  return TELNET_E_OK;
}

telnet_error telnet_parser_parse(telnet_parser* parser,
                                 const telnet_byte* data,
                                 size_t length,
                                 size_t* bytes_used)
{
  if (!parser)
    return TELNET_E_BAD_PARSER;
  
  // Reset the interrupt flag
  parser->interrupted = 0;
  
  // Only bother saving text if it'll be used
  telnet_byte* buf = NULL;
  size_t buflen = 0;
  if (parser->callback)
  {
    // Because of how the parser translates data, a run of text is guaranteed to
    // be at most 'length' characters long. In practice it's usually less, due to
    // escaped characters (IAC IAC -> IAC) and text separated by commands.
    buf = malloc(length * sizeof(*buf));
    if (!buf)
      return TELNET_E_ALLOC;
  }
  
  parser->p = data;
  parser->pe = data + length;
  parser->eof = parser->pe;
  
  %% write exec;
  
  if (bytes_used != NULL)
    *bytes_used = parser->p - data;
  
  free(buf);
  buf = NULL;
  parser->p = parser->pe = parser->eof = NULL;
  
  return (parser->interrupted) ? TELNET_E_INTERRUPT : TELNET_E_OK;
}

telnet_error telnet_parser_interrupt(telnet_parser* parser)
{
  if (!parser)
    return TELNET_E_BAD_PARSER;
  
  // Force the parser to stop where it's at.
  if (parser->p)
    parser->eof = parser->pe = parser->p + 1;
  
  parser->interrupted = 1;
  return TELNET_E_OK;
}
