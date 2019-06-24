#include <stdio.h>

#ifdef _WIN32
#include <windows.h>
#endif

int main(int argc, char ** argv)
{
  printf("Hello, World!\n");

#ifdef _WIN32
  MessageBoxA(NULL, "Hello, World!", "Hello", MB_OK);
#endif
  return 0;
}
