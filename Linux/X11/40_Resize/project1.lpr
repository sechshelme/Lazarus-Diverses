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
    dis: PDisplay;
    win: TWindow;
    Event: TXEvent;
    msg: PChar;
    scr: cint;
    Width: cint = 0;
    Height: cint = 0;
    xwa: TXWindowAttributes;
    gc: TGC;

  begin
    msg := PChar(AMsg);

    // Erstellt die Verbindung zum Server
    dis := XOpenDisplay(nil);
    if (dis = nil) then begin
      WriteLn('[ModalShowX11Window] Kann nicht das Display öffnen');
      exit;
    end;

    scr := DefaultScreen(dis);

    // Erstellt das Fenster
    win := XCreateSimpleWindow(dis, RootWindow(dis, scr), 10, 10, 200, 200, 1, BlackPixel(dis, scr), WhitePixel(dis, scr));

    // winählt die gewünschten Ereignisse aus
    //   XSelectInput(dis, win, ExposureMask or KeyPressMask or ButtonPressMask or StructureNotifyMask or ResizeRedirectMask);
    XSelectInput(dis, win, ExposureMask or KeyPressMask or ButtonPressMask or StructureNotifyMask);

    // Fenster anzeigen
    XMapWindow(dis, win);
    gc:=DefaultGC(dis, scr);

    // Ereignisschleife
    while (True) do begin
      XNextEvent(dis, @Event);

      case Event._type of
        Expose: // Zeichnet ein Rechteck (Quadrat) und gibt einen Text aus
        begin
          //        XGetWindowAttributes(dis, win, @xwa);
          WriteLn('paint');
          //          WriteLn('xwa=', xwa.x, ' ywa=', xwa.y);
          //          WriteLn('x=', Event.xresizerequest.width, ' y=', Event.xresizerequest.height);
          //          WriteLn('xconf=',Event.xconfigure.width, ' xconf=',Event.xconfigure.height);

          XSetForeground(dis, gc, $00);
          XFillRectangle(dis, win, gc, 20, 20, Width - 40, Height - 40);
          XSetForeground(dis, gc, $FF00);
          XDrawString(dis, win, gc, 50, 50, msg, strlen(msg));
        end;
        ResizeRequest: begin
          WriteLn('resize');

        end;
        ConfigureNotify: begin
          Width := Event.xconfigure.Width;
          Height := Event.xconfigure.Height;
          WriteLn('configure');
          //          WriteLn('width: ',Event.xconfigure.width, '   height:',Event.xconfigure.height,'   ');
          WriteLn('x=', Event.xresizerequest.Width, ' y=', Event.xresizerequest.Height);
          XGetWindowAttributes(dis, win, @xwa);
          WriteLn('xwa=', xwa.x, ' ywa=', xwa.y);

        end;
        KeyPress: begin
          WriteLn(XKeysymToString(XLookupKeysym(@Event.xkey, 0)));
          if XLookupKeysym(@Event.xkey, 0) = XK_Escape then // Beendet das Programm bei [ESC]
          begin
            Break;
          end;
        end;
        ButtonPress:
        begin
          WriteLn('x=', Event.xbutton.x, ' y=', Event.xbutton.y);
          WriteLn('xroot=', Event.xbutton.x_root, ' yroot=', Event.xbutton.y_root);
          WriteLn('state=', Event.xbutton.state, ' button=', Event.xbutton.button);
        end;
      end;

    end;
    XCloseDisplay(dis);
  end;

begin
  ModalShowX11Window('Meine Nachricht');
end.
