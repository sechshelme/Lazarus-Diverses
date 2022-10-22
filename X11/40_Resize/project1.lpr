program Project1;

uses
  unixtype,
  ctypes,
  xlib,
  xutil,
  keysym,
  x;

  procedure ModalShowX11Window(AMsg: string);
  var
    d: PDisplay;
    w: TWindow;
    Event: TXEvent;
    msg: PChar;
    s: cint;
    xwa: TXWindowAttributes;


  begin
    msg := PChar(AMsg);

    // Erstellt die Verbindung zum Server
    d := XOpenDisplay(nil);
    if (d = nil) then begin
      WriteLn('[ModalShowX11Window] Kann nicht das Display öffnen');
      exit;
    end;

    s := DefaultScreen(d);

    // Erstellt das Fenster
    w := XCreateSimpleWindow(d, RootWindow(d, s), 10, 10, 200, 200, 1, BlackPixel(d, s), WhitePixel(d, s));

   // Wählt die gewünschten Ereignisse aus
 //   XSelectInput(d, w, ExposureMask or KeyPressMask or ButtonPressMask or StructureNotifyMask or ResizeRedirectMask);
    XSelectInput(d, w, ExposureMask or KeyPressMask or ButtonPressMask or StructureNotifyMask);

    // Fenster anzeigen
    XMapWindow(d, w);

    // Ereignisschleife
    while (True) do begin
      XNextEvent(d, @Event);

      case Event._type of
        Expose: // Zeichnet ein Rechteck (Quadrat) und gibt einen Text aus
        begin
  //        XGetWindowAttributes(d, w, @xwa);
          WriteLn('paint');
//          WriteLn('xwa=', xwa.x, ' ywa=', xwa.y);
//          WriteLn('x=', Event.xresizerequest.width, ' y=', Event.xresizerequest.height);
//          WriteLn('xconf=',Event.xconfigure.width, ' xconf=',Event.xconfigure.height);

          XFillRectangle(d, w, DefaultGC(d, s), 20, 20, 10, 10);
          XDrawString(d, w, DefaultGC(d, s), 50, 50, msg, strlen(msg));
        end;
        ResizeRequest:
        begin
          WriteLn('resize');

        end;
        ConfigureNotify:
        begin
          WriteLn('configure');
          WriteLn('width: ',Event.xconfigure.width, '   height:',Event.xconfigure.height,'   ');
          WriteLn('x=', Event.xresizerequest.width, ' y=', Event.xresizerequest.height);
          XGetWindowAttributes(d, w, @xwa);
          WriteLn('xwa=', xwa.x, ' ywa=', xwa.y);

        end;
        KeyPress: begin
          WriteLn(XKeysymToString(XLookupKeysym(@Event.xkey, 0)));
          if XLookupKeysym(@Event.xkey, 0) = XK_Escape then // Beendet das Programm bei [ESC]
          begin
            Break;
          end;
        end;
        ButtonPress: // Beendet das Programm bei einem Mausklick
        begin

          WriteLn('x=', Event.xbutton.x, ' y=', Event.xbutton.y);
          WriteLn('xroot=', Event.xbutton.x_root, ' yroot=', Event.xbutton.y_root);
          WriteLn('state=', Event.xbutton.state, ' button=', Event.xbutton.button);
        end;
      end;

    end;

    // Schliesst Verbindung zum Server
    XCloseDisplay(d);
  end;

begin
  ModalShowX11Window('Meine Nachricht');
end.
