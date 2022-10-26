program project1;

{$mode objfpc}{$H+}

uses
  xlib,
  x,
  ctypes;

procedure ModalShowX11Window(AMsg: string);
var
  d: PDisplay;
  w: TWindow;
  e: TXEvent;
  msg: PChar;
  s: cint;
begin
  msg := PChar(AMsg);

  { open connection with the server }
  d := XOpenDisplay(nil);
  if (d = nil) then
  begin
    WriteLn('[ModalShowX11Window] Cannot open display');
    exit;
  end;

  s := DefaultScreen(d);

  { create window }
  w := XCreateSimpleWindow(d, RootWindow(d, s), 10, 10, 200, 200, 1,
                           BlackPixel(d, s), WhitePixel(d, s));

  { select kind of events we are interested in }
  XSelectInput(d, w, ExposureMask or KeyPressMask);

  { map (show) the window }
  XMapWindow(d, w);

  { event loop }
  while (True) do
  begin
    XNextEvent(d, @e);
    { draw or redraw the window }
    if (e._type = Expose) then
    begin
      XFillRectangle(d, w, DefaultGC(d, s), 20, 20, 10, 10);
      XDrawString(d, w, DefaultGC(d, s), 50, 50, msg, strlen(msg));
    end;
    { exit on key press }
    if (e._type = KeyPress) then Break;
  end;

  { close connection to server }
  XCloseDisplay(d);
end;

begin
  ModalShowX11Window('My message');
end.
