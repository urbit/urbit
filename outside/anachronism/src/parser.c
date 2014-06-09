
#line 1 "src/parser.rl"
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


#line 53 "src/parser.c"
static const int telnet_parser_start = 7;
static const int telnet_parser_first_final = 7;
static const int telnet_parser_error = -1;

static const int telnet_parser_en_main = 7;


#line 130 "src/parser.rl"


telnet_parser* telnet_parser_new(void* userdata,
                                 telnet_parser_callback callback)
{
  telnet_parser* parser = malloc(sizeof(telnet_parser));
  if (parser)
  {
    memset(parser, 0, sizeof(*parser));
    
#line 72 "src/parser.c"
	{
	 parser->cs = telnet_parser_start;
	}

#line 140 "src/parser.rl"
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
  
  
#line 127 "src/parser.c"
	{
	if ( ( parser->p) == ( parser->pe) )
		goto _test_eof;
	switch (  parser->cs )
	{
tr1:
#line 6 "src/parser_common.rl"
	{( parser->p)--;}
#line 111 "src/parser.rl"
	{
    if (parser->callback && buf != NULL)
    {
      telnet_parser_warning_event ev;
      EV_WARNING(ev, "Invalid \\r: not followed by \\n or \\0.", ( parser->p)-data);
      parser->callback(parser, (telnet_parser_event*)&ev);
    }
  }
#line 57 "src/parser.rl"
	{
    if (parser->callback && buflen > 0)
    {
      telnet_parser_data_event ev;
      EV_DATA(ev, buf, buflen);
      parser->callback(parser, (telnet_parser_event*)&ev);
      buflen = 0;
    }
  }
	goto st7;
tr2:
#line 67 "src/parser.rl"
	{
    if (parser->callback && buf)
      buf[buflen++] = (*( parser->p));
  }
	goto st7;
tr3:
#line 57 "src/parser.rl"
	{
    if (parser->callback && buflen > 0)
    {
      telnet_parser_data_event ev;
      EV_DATA(ev, buf, buflen);
      parser->callback(parser, (telnet_parser_event*)&ev);
      buflen = 0;
    }
  }
#line 72 "src/parser.rl"
	{
    if (parser->callback && buf)
    {
      telnet_parser_command_event ev;
      EV_COMMAND(ev, (*( parser->p)));
      parser->callback(parser, (telnet_parser_event*)&ev);
    }
  }
	goto st7;
tr12:
#line 57 "src/parser.rl"
	{
    if (parser->callback && buflen > 0)
    {
      telnet_parser_data_event ev;
      EV_DATA(ev, buf, buflen);
      parser->callback(parser, (telnet_parser_event*)&ev);
      buflen = 0;
    }
  }
#line 6 "src/parser_common.rl"
	{( parser->p)--;}
#line 119 "src/parser.rl"
	{
    if (parser->callback && buf != NULL)
    {
      telnet_parser_warning_event ev;
      EV_WARNING(ev, "IAC followed by invalid command.", ( parser->p)-data);
      parser->callback(parser, (telnet_parser_event*)&ev);
    }
  }
#line 102 "src/parser.rl"
	{
    if (parser->callback && buf != NULL)
    {
      telnet_parser_subnegotiation_event ev;
      EV_SUBNEGOTIATION(ev, 0, parser->option_mark);
      parser->callback(parser, (telnet_parser_event*)&ev);
    }
  }
	goto st7;
tr13:
#line 57 "src/parser.rl"
	{
    if (parser->callback && buflen > 0)
    {
      telnet_parser_data_event ev;
      EV_DATA(ev, buf, buflen);
      parser->callback(parser, (telnet_parser_event*)&ev);
      buflen = 0;
    }
  }
#line 102 "src/parser.rl"
	{
    if (parser->callback && buf != NULL)
    {
      telnet_parser_subnegotiation_event ev;
      EV_SUBNEGOTIATION(ev, 0, parser->option_mark);
      parser->callback(parser, (telnet_parser_event*)&ev);
    }
  }
	goto st7;
tr14:
#line 84 "src/parser.rl"
	{
    if (parser->callback && buf)
    {
      telnet_parser_option_event ev;
      EV_OPTION(ev, parser->option_mark, (*( parser->p)));
      parser->callback(parser, (telnet_parser_event*)&ev);
    }
  }
	goto st7;
st7:
	if ( ++( parser->p) == ( parser->pe) )
		goto _test_eof7;
case 7:
#line 252 "src/parser.c"
	switch( (*( parser->p)) ) {
		case 13u: goto tr15;
		case 255u: goto st1;
	}
	goto tr2;
tr15:
#line 67 "src/parser.rl"
	{
    if (parser->callback && buf)
      buf[buflen++] = (*( parser->p));
  }
	goto st0;
st0:
	if ( ++( parser->p) == ( parser->pe) )
		goto _test_eof0;
case 0:
#line 269 "src/parser.c"
	switch( (*( parser->p)) ) {
		case 0u: goto st7;
		case 10u: goto tr2;
	}
	goto tr1;
st1:
	if ( ++( parser->p) == ( parser->pe) )
		goto _test_eof1;
case 1:
	switch( (*( parser->p)) ) {
		case 250u: goto tr4;
		case 255u: goto tr2;
	}
	if ( 251u <= (*( parser->p)) && (*( parser->p)) <= 254u )
		goto tr5;
	goto tr3;
tr4:
#line 57 "src/parser.rl"
	{
    if (parser->callback && buflen > 0)
    {
      telnet_parser_data_event ev;
      EV_DATA(ev, buf, buflen);
      parser->callback(parser, (telnet_parser_event*)&ev);
      buflen = 0;
    }
  }
	goto st2;
st2:
	if ( ++( parser->p) == ( parser->pe) )
		goto _test_eof2;
case 2:
#line 302 "src/parser.c"
	goto tr6;
tr11:
#line 6 "src/parser_common.rl"
	{( parser->p)--;}
#line 111 "src/parser.rl"
	{
    if (parser->callback && buf != NULL)
    {
      telnet_parser_warning_event ev;
      EV_WARNING(ev, "Invalid \\r: not followed by \\n or \\0.", ( parser->p)-data);
      parser->callback(parser, (telnet_parser_event*)&ev);
    }
  }
#line 57 "src/parser.rl"
	{
    if (parser->callback && buflen > 0)
    {
      telnet_parser_data_event ev;
      EV_DATA(ev, buf, buflen);
      parser->callback(parser, (telnet_parser_event*)&ev);
      buflen = 0;
    }
  }
	goto st3;
tr7:
#line 67 "src/parser.rl"
	{
    if (parser->callback && buf)
      buf[buflen++] = (*( parser->p));
  }
	goto st3;
tr6:
#line 93 "src/parser.rl"
	{
    parser->option_mark = (*( parser->p));
    if (parser->callback && buf != NULL)
    {
      telnet_parser_subnegotiation_event ev;
      EV_SUBNEGOTIATION(ev, 1, parser->option_mark);
      parser->callback(parser, (telnet_parser_event*)&ev);
    }
  }
	goto st3;
st3:
	if ( ++( parser->p) == ( parser->pe) )
		goto _test_eof3;
case 3:
#line 350 "src/parser.c"
	switch( (*( parser->p)) ) {
		case 13u: goto tr8;
		case 255u: goto st5;
	}
	goto tr7;
tr8:
#line 67 "src/parser.rl"
	{
    if (parser->callback && buf)
      buf[buflen++] = (*( parser->p));
  }
	goto st4;
st4:
	if ( ++( parser->p) == ( parser->pe) )
		goto _test_eof4;
case 4:
#line 367 "src/parser.c"
	switch( (*( parser->p)) ) {
		case 0u: goto st3;
		case 10u: goto tr7;
	}
	goto tr11;
st5:
	if ( ++( parser->p) == ( parser->pe) )
		goto _test_eof5;
case 5:
	switch( (*( parser->p)) ) {
		case 240u: goto tr13;
		case 255u: goto tr7;
	}
	goto tr12;
tr5:
#line 57 "src/parser.rl"
	{
    if (parser->callback && buflen > 0)
    {
      telnet_parser_data_event ev;
      EV_DATA(ev, buf, buflen);
      parser->callback(parser, (telnet_parser_event*)&ev);
      buflen = 0;
    }
  }
#line 81 "src/parser.rl"
	{
    parser->option_mark = (*( parser->p));
  }
	goto st6;
st6:
	if ( ++( parser->p) == ( parser->pe) )
		goto _test_eof6;
case 6:
#line 402 "src/parser.c"
	goto tr14;
	}
	_test_eof7:  parser->cs = 7; goto _test_eof; 
	_test_eof0:  parser->cs = 0; goto _test_eof; 
	_test_eof1:  parser->cs = 1; goto _test_eof; 
	_test_eof2:  parser->cs = 2; goto _test_eof; 
	_test_eof3:  parser->cs = 3; goto _test_eof; 
	_test_eof4:  parser->cs = 4; goto _test_eof; 
	_test_eof5:  parser->cs = 5; goto _test_eof; 
	_test_eof6:  parser->cs = 6; goto _test_eof; 

	_test_eof: {}
	if ( ( parser->p) == ( parser->eof) )
	{
	switch (  parser->cs ) {
	case 3: 
	case 7: 
#line 57 "src/parser.rl"
	{
    if (parser->callback && buflen > 0)
    {
      telnet_parser_data_event ev;
      EV_DATA(ev, buf, buflen);
      parser->callback(parser, (telnet_parser_event*)&ev);
      buflen = 0;
    }
  }
	break;
#line 431 "src/parser.c"
	}
	}

	}

#line 189 "src/parser.rl"
  
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
