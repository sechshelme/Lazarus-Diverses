program project1;

uses
  Windows;

  procedure Cls;
  var
    numChars: integer = 80 * 25;
    c: coord;
    x: longword = 0;
  begin
    c.X := 0;
    c.Y := 0;
    FillConsoleOutputAttribute(GetStdHandle(STD_OUTPUT_HANDLE), $13,
      numChars, c, x);
    FillConsoleOutputCharacter(GetStdHandle(STD_OUTPUT_HANDLE), char(13),
      numChars, c, x);
  end;

  procedure Font;
  var
    hBuff: Handle;
    i: SECURITY_ATTRIBUTES;
  begin
    hBuff := CreateConsoleScreenBuffer(GENERIC_READ or GENERIC_WRITE, 0, i, CONSOLE_TEXTMODE_BUFFER, nil);
  end;

  procedure OutCharXY(x, y: integer; ch: char; col: byte);
  var
    numChars: integer = 1;
    c: coord;
    p: longword = 0;
  begin
    c.X := x;
    c.Y := y;
    FillConsoleOutputAttribute(GetStdHandle(STD_OUTPUT_HANDLE), col, numChars, c, p);
    FillConsoleOutputCharacter(GetStdHandle(STD_OUTPUT_HANDLE), ch, numChars, c, p);
  end;


var
  i: integer;
begin
    cls;
  for i := 0 to 255 do begin
    OutCharXY(i mod 16 * 2, i div 16, char(i), 7);
  end;
  readln;
end.
