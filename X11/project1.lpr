program project1;

uses
  unixtype,
  xlib,
  xutil,
  x;

var
  d: PDisplay;
  window_name: string = 'Einfaches Fenster - hallo_x';
  rw: TWindow;
  s: cint;
  sh: TXSizeHints;
begin
  d := XOpenDisplay(nil);
  if d = nil then begin
    WriteLn('Verbindung zum X-Server fehlgeschlagen?!?');
  end;

  s := XDefaultScreen(d);
  rw := RootWindow(d, s);
  XCreateSimpleWindow(d, rw, 10, 10, 400, 300, 5, BlackPixel(d, s), WhitePixel(d, s));
  sh.flags := PSize or PMinSize or PMaxSize;
  sh.min_width := 400;
  sh.max_width := 400;
  sh.min_height := 300;
  sh.max_height := 300;

  XSetStandardProperties(d, rw, PChar(window_name), 'icon', 0, nil, 0, @sh);

  XMapWindow(d, rw);
  ReadLn;
end.
