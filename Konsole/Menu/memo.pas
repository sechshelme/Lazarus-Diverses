unit Memo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crt, Shares;

type

  { TMemo }

  TMemo = class(TObject)
    Fensterpos: TPoint;
    breite: integer;
    col: byte;
  private
    Text: array of string;
  public
    constructor Create;
    constructor Create(x, y: integer);
    procedure Add(s: string);
    procedure Add();
    procedure Paint;
    function Run: byte;
    procedure SetColor(c: byte);
  end;


implementation

{ TMemo }

constructor TMemo.Create;
begin
  Create(2, 2);
end;

constructor TMemo.Create(x, y: integer);
begin
  inherited Create;
  breite := 4;
  col := $16;
  Fensterpos.x := x;
  Fensterpos.y := y;
end;

procedure TMemo.Add(s: string);
begin
  if Length(s) > breite then begin
    breite := Length(s);
  end;
  SetLength(Text, Length(Text) + 1);
  Text[Length(Text) - 1] := s;
end;

procedure TMemo.Add;
begin
  Add('');
end;

procedure TMemo.Paint;
var
  i: integer;
begin
  with Fensterpos do begin
    TextAttr := col;
    Rahmen(x, y, x + breite + 3, y + Length(Text) + 3, 2);
  end;
  GotoXY(Fensterpos.x + 1, Fensterpos.y + 1);
  Write(StringOfChar(' ', breite + 2));
  GotoXY(Fensterpos.x + 1, Fensterpos.y + 2 + Length(Text));
  Write(StringOfChar(' ', breite + 2));
  for i := 0 to Length(Text) - 1 do begin
    GotoXY(Fensterpos.x + 1, Fensterpos.y + 2 + i);
    Write(' ', Text[i], StringOfChar(' ', breite - Length(Text[i]) + 1));
  end;
end;

function TMemo.Run: byte;
var
  ch: char;
begin
  Paint;
  repeat
  until Keypressed;
  ch := ReadKey;
end;

procedure TMemo.SetColor(c: byte);
begin
  col := c;
end;

end.
