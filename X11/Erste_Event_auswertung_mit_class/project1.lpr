program project1;

uses
  unixtype,
  ctypes,
  xlib,
  xutil,
  x;

type

  { TMyWin }

  TMyWin = class(TObject)
  private
    display: PDisplay;
    screen: cint;
    depth: cint;
    rootwin, win: TWindow;
  public
    constructor Create;
    destructor Destroy; override;
    procedure prcess_event(report: TXEvent);
    procedure eventloop;
  end;

  { TMyWin }

  constructor TMyWin.Create;
  const
    window_name: PChar = 'Einfaches Fenster - hallo_x';
    icon_name: PChar = 'window';
  var
    Width, Height: cuint;
    size_hints: TXSizeHints;
  begin
    Width := 400;
    Height := 300;
    display := XOpenDisplay(nil);
    if display = nil then begin
      WriteLn('Verbindung zum X-Server fehlgeschlagen?!?');
      Exit;
    end;
    screen := XDefaultScreen(display);
    depth := XDefaultDepth(display, screen);
    rootwin := RootWindow(display, screen);
    win := XCreateSimpleWindow(display, rootwin, 400, 400, Width, Height, 5, BlackPixel(display, screen), WhitePixel(display, screen));

    size_hints.flags := PSize or PMinSize or PMaxSize;
    size_hints.min_width := Width;
    size_hints.max_width := Width;
    size_hints.min_height := Height;
    size_hints.max_height := Height;
    XSetStandardProperties(display, win, window_name, icon_name, None, nil, 0, @size_hints);
    // Erwünschte Events, auf die das Fenster reagieren soll ...
    XSelectInput(display, win, ButtonPressMask or KeyPressMask);
    // Fenster anzeigen
    XMapWindow(display, win);
    //  Result := 1;
  end;

  destructor TMyWin.Destroy;
  begin
    XDestroyWindow(display, win);
    XCloseDisplay(display);
    inherited Destroy;
  end;

  procedure TMyWin.prcess_event(report: TXEvent);
  var
    key: TKeySym;
  begin
    case report._type of
      KeyPress: begin
        key := XLookupKeysym(@report.xkey, 0);
        if key <> 0 then begin
          WriteLn('Tastatur Event');
        end;
      end;
      ButtonPressMask: begin
        WriteLn('Maus Event');
      end;
    end;
  end;

  procedure TMyWin.eventloop;
  var
    num_events: cint;
    xev: TXEvent;
  begin
    XFlush(display);
    num_events := XPending(display);
    while num_events <> 0 do begin
      Dec(num_events);
      XNextEvent(display, @xev);
      prcess_event(xev);
    end;
  end;

var
  MyWin: TMyWin;

begin
  MyWin := TMyWin.Create;
  repeat
    MyWin.eventloop;
  until False;
  MyWin.Destroy;
end.
