program Project1;

uses
heaptrc,  sdl;

var
  scr, img: PSDL_Surface; // Our main screen
  Quit: boolean;
  Event: TSDL_Event;
  dstrect: TSDL_Rect;
  col: uint32;
  ofs: TSDL_Rect;

  procedure apply_surface(x,y:Integer; source: PSDL_Surface; destination:PSDL_Surface);
  var
    offset: SDL_Rect;
  begin
    offset.x:=x;
    offset.y:=y;
    SDL_BlitSurface(source,nil,destination,@offset);
  end;


begin
  // Initialize the video SDL subsystem
  if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
    Writeln('SDL could not initialize! SDL_Error: ', SDL_GetError);
  end;
  scr := SDL_SetVideoMode(640, 480, 32, SDL_SWSURFACE);
  if scr = nil then  begin
    WriteLn('error');
  end;
  SDL_WM_SetCaption('SDL-Fenster', nil);
  img := SDL_LoadBMP('icon.bmp');
  if img = nil then begin
    WriteLn('error');
  end;

  if scr = nil then begin
    WriteLn('could not initialize sdl2: ', SDL_GetError());
  end;
  repeat
    SDL_WaitEvent(@Event);
    case Event.type_ of
      SDL_QUITEV: begin
        Quit := True;
      end;
      SDL_KEYDOWN: begin
        ofs.x := 100;
        ofs.y := 100;
        SDL_Flip(img);
        SDL_BlitSurface(img, nil, scr, @ofs);
        SDL_Flip(scr);


        apply_surface( 320, 0, img, scr );
          apply_surface( 0, 240, img, scr );
          apply_surface( 320, 240, img, scr );

        dstrect.h := 100;
        dstrect.w := 100;
        SDL_FillRect(scr, @dstrect, $BBBBBB);
        SDL_Flip(scr);
      end;
    end;

  until Quit;

  SDL_FreeSurface(img);
  SDL_FreeSurface(scr);


  SDL_Quit; // close the subsystems and SDL
end.
