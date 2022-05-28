program project1;

uses
  Crt;

var
  f: Text;
  c: char;
begin
  AssignFile(f, '');
  ReWrite(f);
  WriteLn(f, 'Kann umgelenkt werden');
  GotoXY(20, 10);
  TextAttr := $64;
  WriteLn('Ausgabe immmer auf Konsole');
  repeat
    c := Readkey;
    textAttr := 16;
    Write(f, c);
  until c = #27;
  CloseFile(f);
end.
