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
    XSelectInput(d, w, ExposureMask or KeyPressMask or ButtonPressMask);

    // Fenster anzeigen
    XMapWindow(d, w);

    // Ereignisschleife
    while (True) do begin
      XNextEvent(d, @Event);

      case Event._type of
        Expose: // Zeichnet ein Rechteck (Quadrat) und gibt einen Text aus
        begin
          XFillRectangle(d, w, DefaultGC(d, s), 20, 20, 10, 10);
          XDrawString(d, w, DefaultGC(d, s), 50, 50, msg, strlen(msg));
        end;
        KeyPress: begin
          // Beendet das Programm bei [ESC]
          if XLookupKeysym(@Event.xkey, 0) = XK_Escape then begin
            Break;
          end;
        end;
        ButtonPress: // Beendet das Programm bei einem Mausklick
        begin
          //          Break;
        end;
      end;

    end;

    // Schliesst Verbindung zum Server
    XCloseDisplay(d);
  end;

begin
  ModalShowX11Window('Meine Nachricht');
end.
