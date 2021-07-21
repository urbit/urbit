#include <stdio.h>
#include <string.h>

enum { INIT, CPAR, DQ, DQS, SQ, SQS };
char line[1 << 16];

/* seh_handler_decorator: registers u3_exception_handler for all functions in given source file
*/
int main(int argc, const char* argv[])
{
    if (argc != 2)
        return 1;

    int c, state = INIT, curly = 0, emit = 0;

    while (fgets(line, sizeof(line), stdin))
    {
        if (line[0] == '#')
        {
            emit = !!strstr(line, argv[1]);
            fputs(line, stdout);
        }
        else
        for (int i = 0; line[i]; i++)
        {
            switch (state) {
            case INIT:
            case CPAR:
                switch (line[i]) {
                case ' ':
                case '\t':
                case '\n':
                case '\r':
                case '\f': break;
                case '{':  curly++; if (emit && curly == 1 && state == CPAR) goto emit_handler; goto reset;
                case '}':  curly--; goto reset;
                case '"':  state = DQ;   break;
                case '\'': state = SQ;   break;
                case ')':  state = CPAR; break;
                reset:
                default:   state = INIT; break;
                } break;
            case DQ:
                switch (line[i]) {
                case '\\': state = DQS;  break;
                case '"':  state = INIT; break;
                } break;
            case DQS:      state = DQ;   break;
            case SQ:
                switch (line[i]) {
                case '\\': state = SQS;  break;
                case '\'': state = INIT; break;
                } break;
            case SQS:      state = SQ;   break;
            }
            fputc(line[i], stdout);
            continue;
        emit_handler:
            fputs("{__asm__(\".seh_handler _mingw_exception_filter,@except\\n\");", stdout);
            state = INIT;
        }
    }

    return 0;
}
