program project1;

uses
  Windows;

  // https://stackoverflow.com/questions/73322716/setclipboarddata-and-new-operator-for-handle

const
  hello: string = 'Hello World !';
var
  h: HGLOBAL;
  pmem: pchar;
begin
  WriteLn('Clipboard Demo');
  h := GlobalAlloc(GMEM_MOVEABLE, Length(hello)+1);
  if h <> 0 then begin
    pmem := PChar(GlobalLock(h));
    move(hello[1], pmem[0], Length(hello));
    GlobalUnlock(h);
    if OpenClipboard(0) then begin
      EmptyClipboard;
      if SetClipboardData(CF_TEXT, h) <> 0 then begin
        h := 0;
      end;
      CloseClipboard;
    end;
    if h <> 0 then begin
      GlobalFree(h);
    end;
  end;
end.

