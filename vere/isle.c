/* vere/isle.c
**
*/
#include <SDL2/SDL.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <uv.h>
#include <errno.h>
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "all.h"
#include "vere/vere.h"

/* u3_ames_ef_bake(): notify %ames that we're live.
*/
void
u3_isle_ef_bake(void)
{
  return;
}

/* u3_ames_ef_send(): send packet to network (v4).
*/
void
u3_isle_ef_send(u3_noun lan, u3_noun pac)
{
  return;
}

/* u3_ames_ef_turf(): initialize ames I/O on domain(s).
*/
void
u3_isle_ef_turf(u3_noun tuf)
{
  return;
}

SDL_Window * gWindow = NULL;
SDL_Surface * gScreenSurface = NULL;
SDL_Surface * gHelloWorld = NULL;
SDL_Renderer * gRenderer = NULL;
/* u3_ames_io_init(): initialize ames I/O.
*/
void
u3_isle_io_init()
{
{
  printf("isle_io_init\n");

	//Initialize SDL
	if( SDL_Init( SDL_INIT_VIDEO ) < 0 )
	{
		printf( "SDL could not initialize! SDL_Error: %s\n", SDL_GetError() );
    exit(1);
	}
	else
	{
		//Create window
		gWindow = SDL_CreateWindow( "SDL Tutorial", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 640, 480, SDL_WINDOW_SHOWN); 
		if( gWindow == NULL )
		{
			printf( "Window could not be created! SDL_Error: %s\n", SDL_GetError() );
      exit(1);
		}
		else
		{
			//Get window surface
			gScreenSurface = SDL_GetWindowSurface( gWindow );

	    gHelloWorld = SDL_LoadBMP( "/Users/isaac/hello_world.bmp" );
      if( gHelloWorld == NULL )
      {
        printf( "Unable to load image %s! SDL Error: %s\n", "hello_world.bmp", SDL_GetError() );
        exit(1);
      }
      else
      {
        //Apply the image
        SDL_BlitSurface( gHelloWorld, NULL, gScreenSurface, NULL );
        
        //Update the surface
        SDL_UpdateWindowSurface( gWindow );

        //Wait two seconds
      }
		}
	}

 


  }
  return;
}

/* u3_ames_io_talk(): start receiving ames traffic.
*/
void
u3_isle_io_talk()
{
  return;
}

/* u3_ames_io_exit(): terminate ames I/O.
*/
void
u3_isle_io_exit()
{
  return;
}
