program Project1;
uses
  SysUtils,
  Linux,
  BaseUnix;
var
  i, fd: integer;
begin
  WriteLn('Abruch mit [^C]');
  fd := FpOpen('/dev/console', O_RDONLY);
  i := 0;
  repeat
    if i > 7 then begin
      i := 0;
    end;
    FpIOCtl(fd, KDSETLED, Pointer(i));
    i := i + 1;
    Sleep(300);
  until False;
  FpClose(fd);
end.
