program Generic ;
//{$R GENERIC}
uses
  Windows;

var
  wnd, hStatic, hEdit, hButton: HWND;
  Instance: HINST;
  ncmd: longint;
  LWndClass: WNDClass;
  Ms: TMsg;
  hFontText, hFontButton: HFONT;

  function Proc(wnd: HWND; msg: UINT; wpara: WPARAM; lpara: LPARAM): LRESULT; stdcall;
  var
    ps: PAINTSTRUCT;
    dc: HDC;
    rc: Rect;
  begin
    case msg of
      WM_DESTROY: begin
      end;

      WM_PAINT: begin
        dc := BeginPaint(wnd, ps);

        GetClientRect(wnd, rc);
        SetTextColor(dc, 0);
        SetBkMode(dc, TRANSPARENT);
        DrawText(dc, 'HELLO WORLD', -1, rc, DT_CENTER or DT_SINGLELINE or DT_VCENTER);

        EndPaint(wnd, @ps);
      end;
      else begin
        DefWindowProc(wnd, msg, wpara, lpara);
      end;
    end;

  end;

begin

  LWndClass.hInstance := hInstance;
  with LWndClass do begin
    lpszClassName := 'MyWinApiWnd';
    Style := CS_PARENTDC or CS_BYTEALIGNCLIENT;
    hIcon := LoadIcon(hInstance, 'MAINICON');
    lpfnWndProc := @proc;
    hbrBackground := COLOR_BTNFACE + 1;
    hCursor := LoadCursor(0, IDC_ARROW);
  end;

  RegisterClass(LWndClass);


wnd := CreateWindow(LWndClass.lpszClassName, 'Window Title', WS_CAPTION or WS_MINIMIZEBOX or WS_SYSMENU or WS_VISIBLE, (GetSystemMetrics(SM_CXSCREEN) div 2) - 190, (GetSystemMetrics(SM_CYSCREEN) div 2) - 170, 386, 200, 0, 0, hInstance, nil);

  //message loop
  while GetMessage(Ms, 0, 0, 0) do begin
    TranslateMessage(Ms);
    DispatchMessage(Ms);
  end;end.
