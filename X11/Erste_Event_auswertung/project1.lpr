program project1;

uses
  unixtype,
  ctypes,
  xlib,
  xutil,
  x;

var
  display: PDisplay;
  screen: cint;
  depth: cint;
  rootwin, win: TWindow;

  function create_window: integer;
  const
    window_name: PChar = 'Einfaches Fenster - hallo_x';
    icon_name: PChar = 'window';
  var
    width, height: cuint;
    size_hints: TXSizeHints;
  begin
    width := 400;
    height := 300;
    display := XOpenDisplay(nil);
    if display = nil then begin
      WriteLn('Verbindung zum X-Server fehlgeschlagen?!?');
      Exit;
    end;
    screen := XDefaultScreen(display);
    depth := XDefaultDepth(display, screen);
    rootwin := RootWindow(display, screen);
    win := XCreateSimpleWindow(display, rootwin, 400, 400, width, height, 5, BlackPixel(display, screen), WhitePixel(display, screen));

    size_hints.flags := PSize or PMinSize or PMaxSize;
    size_hints.min_width := width;
    size_hints.max_width := width;
    size_hints.min_height := height;
    size_hints.max_height := height;
    XSetStandardProperties(display, win, window_name, icon_name, None, nil, 0, @size_hints);
    // Erwünschte Events, auf die das Fenster reagieren soll ...
    XSelectInput(display, win, ButtonPressMask or KeyPressMask);
    // Fenster anzeigen
    XMapWindow(display, win);
    Result := 1;
  end;

  procedure process_event(report: TXEvent);
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

  procedure close_window;
  begin
    XDestroyWindow(display, win);
    XCloseDisplay(display);
  end;

  procedure eventloop;
  var
    num_events: cint;
    xev: TXEvent;
  begin
    XFlush(display);
    num_events := XPending(display);
    while num_events <> 0 do begin
      Dec(num_events);
      XNextEvent(display, @xev);
      process_event(xev);
    end;
  end;

begin
  create_window;
  repeat
    eventloop;
  until False;
  close_window;
end.
