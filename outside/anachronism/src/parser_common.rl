%%{
  machine telnet_parser_common;
  alphtype unsigned char;
  
  # Shorthand for tidiness.
  action fhold {fhold;}
  
  # Special bytes that must be handled differently from normal text:
  CR = "\r"; # Only \0 or \n may follow
  IAC = 255; # Telnet command marker
  special_byte = CR | IAC;
  
  # The only bytes that may follow a CR:
  NL = "\n";
  NUL = "\0";
  
  # The only bytes that may follow an IAC:
  SE   = 240;
  NOP  = 241;
  DM   = 242;
  BRK  = 243;
  IP   = 244;
  AO   = 245;
  AYT  = 246;
  EC   = 247;
  EL   = 248;
  GA   = 249;
  SB   = 250;
  WILL = 251;
  WONT = 252;
  DO   = 253;
  DONT = 254;
  # IAC IAC is interpreted as a plain-text IAC byte.
  
  # Sorting the above IAC commands by type:
  iac_option_type  = WILL | WONT | DO | DONT;
  iac_subneg_type  = SB;
  iac_command_type = ^(iac_option_type | iac_subneg_type | IAC);
  
  ###
  # Plain text
  ###
  plain_text = (^special_byte) @char;
  cr_seq = CR @char
             ( NUL
             | NL @char
             | ^(NUL|NL) @fhold @warning_cr @flush_text
             );
  
  ###
  # IAC sequence
  ###
  iac_command = iac_command_type @basic_command;
  
  iac_option = iac_option_type @option_mark
               any @option_command;
  
  iac_subneg = iac_subneg_type any @subneg_command
               ( plain_text
               | cr_seq
               | IAC IAC @char
               )** %/flush_text
               IAC
               ( SE
               | ^(IAC|SE) @fhold @warning_iac
               ) >flush_text @subneg_command_end;
  
  iac_seq = ( iac_command
            | iac_option
            | iac_subneg
            );
  
  ###
  # Telnet stream
  ###
  telnet_stream = ( plain_text
                  | cr_seq
                  | IAC
                    ( IAC @char
                    | iac_seq >flush_text
                    )
                  )** %/flush_text;
  
  main := telnet_stream;
}%%
