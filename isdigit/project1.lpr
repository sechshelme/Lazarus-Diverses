program project1;

uses
  Character,  ctypes;

const
  lib_stdio = 'c';
var
  i: integer;
  wc: WideChar;

  function c_isdigit(i: cint): Boolean32; cdecl; external lib_stdio Name 'isdigit';

  function my_isdigit(ch: char): boolean;
  begin
    Result := ch in ['0'..'9'];
  end;

begin
  for i := 32 to 126 do begin
    WriteLn('ch:', char(i), '  c:', c_isdigit(i): 6, '  my:', my_isdigit(char(i)): 6, '  fpc:', IsDigit(char(i)): 6);
  end;

  wc:=UTF8Decode('Ⅺ')[1];
  WriteLn(IsNumber(wc));
  WriteLn(IsDigit(wc));

  wc:=UTF8Decode('Ⅿ')[1];
  WriteLn(IsNumber(wc));
  WriteLn(IsDigit(wc));

  wc:=UTF8Decode('M')[1];
  WriteLn(IsNumber(wc));
  WriteLn(IsDigit(wc));



end.
