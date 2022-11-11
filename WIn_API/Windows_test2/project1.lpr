program project1;

{$mode objfpc}
{$ASMMODE intel}

uses
  Windows,
  Messages,
  SysUtils;

var
  Msg: TMSG;
  LWndClass: TWndClass;
  hMainHandle: HWND;
  hButton: HWND;
  hStatic: HWND;
  hEdit: HWND;
  hFontText: HWND;
  hFontButton: HWND;

  procedure ReleaseResources;
  begin
    DestroyWindow(hButton);
    DestroyWindow(hStatic);
    DestroyWindow(hEdit);
    DeleteObject(hFontText);
    DeleteObject(hFontButton);
    PostQuitMessage(0);
  end;

  function proc(wnd: HWND; Msg: UINT; wpara: WPARAM; lpara: LPARAM): LRESULT; stdcall;
  begin
    case Msg of
      WM_COMMAND: begin
        if lPara = hButton then begin
          MessageBox(hMainHandle, 'You pressed the button Hello', 'Hello', MB_OK or MB_ICONINFORMATION);
        end;
      end;
      WM_DESTROY: begin
        ReleaseResources;
      end;
    end;
    Result := DefWindowProc(wnd, Msg, wPara, lPara);
  end;


begin
  //create the window
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
  hMainHandle := CreateWindow(LWndClass.lpszClassName, 'Window Title', WS_CAPTION or WS_MINIMIZEBOX or WS_SYSMENU or WS_VISIBLE, (GetSystemMetrics(SM_CXSCREEN) div 2) - 190, (GetSystemMetrics(SM_CYSCREEN) div 2) - 170, 386, 200, 0, 0, hInstance, nil);

  //Create the fonts to use
  hFontText := CreateFont(-14, 0, 0, 0, 0, 0, 0, 0, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, VARIABLE_PITCH or FF_SWISS, 'Tahoma');
  hFontButton := CreateFont(-14, 0, 0, 0, 0, 0, 0, 0, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, VARIABLE_PITCH or FF_SWISS, 'Tahoma');

  //create the static
  hStatic := CreateWindow('Static', 'This is static text, like a TLabel', WS_VISIBLE or WS_CHILD or SS_LEFT, 10, 10, 360, 44, hMainHandle, 0, hInstance, nil);
  SendMessage(hStatic, WM_SETFONT, hFontText, 0);

  //create the edit
  hEdit := CreateWindowEx(WS_EX_CLIENTEDGE, 'Edit', 'This a Edit like and TEdit', WS_VISIBLE or WS_CHILD or ES_LEFT or ES_AUTOHSCROLL, 10, 35, 360, 23, hMainHandle, 0, hInstance, nil);
  SendMessage(hEdit, WM_SETFONT, hFontText, 0);

  //create the button
  hButton := CreateWindow('Button', 'Hello', WS_VISIBLE or WS_CHILD or BS_PUSHBUTTON or BS_TEXT, 10, 130, 100, 28, hMainHandle, 0, hInstance, nil);
  SendMessage(hButton, WM_SETFONT, hFontButton, 0);

  //message loop
  while GetMessage(Msg, 0, 0, 0) do begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;

end.
