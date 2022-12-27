program Project1;

uses
  sdl,
  sdl_image;

const
  Screen_Width = 640;
  Screen_Heigth = 480;
  Screen_BPP = 32;

var
  message, background, screen: PSDL_Surface;

  function load_image(const filename: string): PSDL_Surface;
  var
    loadedImage: PSDL_Surface;
  begin
    Result := nil;
    loadedImage := IMG_Load(PChar(filename));
    if loadedImage <> nil then begin
      Result := SDL_DisplayFormat(loadedImage);
      SDL_FreeSurface(loadedImage);
    end else begin
      WriteLn('Kann Datei ' + filename + ' nicht laden');
    end;
  end;

  procedure apply_surface(x, y: integer; Source, destination: PSDL_Surface);
  var
    offset: SDL_Rect;
  begin
    offset.x := x;
    offset.y := y;
    SDL_BlitSurface(Source, nil, destination, @offset);
  end;

begin
  // Start SDL
  if SDL_Init(SDL_INIT_EVERYTHING) < 0 then begin
    Writeln('Kann SDL nicht öffnen: ', SDL_GetError);
    Halt(1);
  end;

  // Screen Setup
  screen := SDL_SetVideoMode(Screen_Width, Screen_Heigth, Screen_BPP, SDL_SWSURFACE);
  if screen = nil then begin
    Writeln('Kann kein Fenster öffnen: ', SDL_GetError);
    Halt(1);
  end;

  SDL_WM_SetCaption('Hello World !', nil);

  // Load Images
  background := load_image('SDL_logo.bmp');
  message := load_image('loggo.png');

  // Copy Image auf Screen
  apply_surface(0, 0, background, screen);
  apply_surface(320, 0, background, screen);
  apply_surface(0, 240, background, screen);
  apply_surface(320, 240, background, screen);

  apply_surface(180, 140, message, screen);

  // Update Screen
  SDL_Flip(screen);

  // Pause 5sek
  SDL_Delay(5000);

  // Images freigeben
  SDL_FreeSurface(message);
  SDL_FreeSurface(background);

  // SDL beenden
  SDL_Quit;
end.
