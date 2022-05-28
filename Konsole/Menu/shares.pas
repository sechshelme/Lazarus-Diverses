unit Shares;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crt;

type
  TPoint = record
    x, y: integer;
  end;

procedure Rahmen(x1, y1, x2, y2: integer; l: byte);

implementation

procedure Rahmen(x1, y1, x2, y2: integer; l: byte);
var
  i: integer;
const
  z: array[1..2] of array[0..5] of char = ((#218, #196, #191, #179, #192, #217), (#201, #205, #187, #186, #200, #188));
begin
  if not (l in [1..2]) then begin
    l := 2;
  end;
  GotoXY(x1, y1);
  Write(z[l, 0]);
  Write(StringOfChar(z[l, 1], x2 - x1 - 1));
  Write(z[l, 2]);
  for i := 1 to y2 - y1 - 1 do begin
    GotoXY(x1, y1 + i);
    Write(z[l, 3]);
    GotoXY(x2, y1 + i);
    Write(z[l, 3]);
  end;
  GotoXY(x1, y2);
  Write(z[l, 4]);
  Write(StringOfChar(z[l, 1], x2 - x1 - 1));
  Write(z[l, 5]);
end;

end.
