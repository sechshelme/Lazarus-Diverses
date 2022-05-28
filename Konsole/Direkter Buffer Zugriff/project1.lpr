program project1;

uses
  Windows, Classes;

var
  x, y: integer;
  wHnd, rHnd: HANDLE;
const
  Width = 80;
  Height = 25;
  bufferSize: COORD = (x: Width; y: Height);
  characterBufferSize: COORD = (x: Width; y: Height);
  characterPosition: COORD = (x: 0; y: 0);
  consoleWriteArea: SMALL_RECT = (Left: 0; Top: 0; Right: Width - 1; Bottom: Height - 1);
var
  consoleBuffer: array[0..Width * 2 * Height] of CHAR_INFO;
  a: byte;
  r: small_Rect;
begin
  wHnd := GetStdHandle(STD_OUTPUT_HANDLE);
  rHnd := GetStdHandle(STD_INPUT_HANDLE);

  SetConsoleTitle('Mein Konsolen Programm');

  r.Left := 0;
  r.Top := 0;

  r.Bottom := 54;
  r.Right := 79;
  SetConsoleWindowInfo(wHnd, True, r);

  SetConsoleScreenBufferSize(wHnd, bufferSize);

  for y := 0 to Height - 1 do begin
    for x := 0 to Width - 1 do begin
      consoleBuffer[x + (Width + 20) * y].AsciiChar := char(x);
      a := y;
      a := a or $f0;
      consoleBuffer[x + (Width + 20) * y].Attributes := a;
    end;
  end;
  characterBufferSize.X := Width + 20;
  //  characterBufferSize.Y:=5;
  //  characterPosition.X:=32;
  //  characterPosition.Y:=4;
  consoleWriteArea.Left := 1;
  consoleWriteArea.Top := 1;
  consoleWriteArea.Right := 60;
  consoleWriteArea.Bottom := 40;

  WriteConsoleOutputA(wHnd, @consoleBuffer, characterBufferSize,
    characterPosition, consoleWriteArea);

  readln;
end.
