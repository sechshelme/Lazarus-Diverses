program Project1;

uses
  sdl;

var
  scr: PSDL_Surface; // Our main screen
  Quit:Boolean;
  Event: TSDL_Event;

begin
  SDL_Init(SDL_INIT_VIDEO); // Initialize the video SDL subsystem
  scr := SDL_SetVideoMode(640, 480, 8, SDL_SWSURFACE);
  SDL_WM_SetCaption('SDL-Fenster', nil);

  if scr = nil then
  begin
    WriteLn('could not initialize sdl2: ', SDL_GetError());
  end;
  repeat
    SDL_WaitEvent(@Event);
    if Event.type_=SDL_QUITEV then Quit:=True;

  until Quit;


  SDL_Quit; // close the subsystems and SDL
end.
