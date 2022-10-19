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
  i:Integer;
  gc: TGC;

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
  w := XCreateSimpleWindow(d, RootWindow(d, s), 10, 10, 200, 200, 1, BlackPixel(d, s), WhitePixel(d, s));

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
      gc:=              DefaultGC(d, s);

      XSetForeground(d, gc, $FF00);
      XFillRectangle(d, w, gc, 20, 20, 50, 50);

      XSetForeground(d, gc, $00);
      XDrawString(d, w, gc, 100, 100, msg, strlen(msg));
    end;
    inc(i);
    { exit on key press }
    if (e._type = KeyPress) then Break;
    WriteLn(i);
  end;

  { close connection to server }
  XCloseDisplay(d);
end;

begin
  ModalShowX11Window('My message');
end.
