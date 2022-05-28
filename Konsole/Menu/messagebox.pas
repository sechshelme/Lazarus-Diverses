unit MessageBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crt, Shares;

type

  { TMessageBox }

  TMessageBox = class(TObject)
  private
    Fensterpos: TPoint;
    breite: integer;
    col: byte;
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

{ TMessageBox }

constructor TMessageBox.Create;
begin
  Create(15, 4);
end;

constructor TMessageBox.Create(x, y: integer);
begin
  inherited Create;
  breite := 4;
  col := $1B;
  Fensterpos.x := x;
  Fensterpos.y := y;
end;

procedure TMessageBox.Add(s: string);
begin
  if Length(s) > breite then begin
    breite := Length(s);
  end;
  SetLength(Text, Length(Text) + 1);
  Text[Length(Text) - 1] := s;
end;

procedure TMessageBox.Add;
begin
  Add('');
end;

procedure TMessageBox.Paint;
var
  i: integer;
begin
  with Fensterpos do begin
    TextAttr := col;
    Rahmen(x, y, x + breite + 3, y + Length(Text) + 5, 2);
  end;
  GotoXY(Fensterpos.x + 1, Fensterpos.y + 1);
  Write(StringOfChar(' ', breite + 2));
  for i := 2 to 4 do begin
    GotoXY(Fensterpos.x + 1, Fensterpos.y + i + Length(Text));
    Write(StringOfChar(' ', breite + 2));
  end;
  GotoXY(fensterpos.x + breite div 2, Fensterpos.y + Length(Text) + 3);
  Write('<Ok>');
  for i := 0 to Length(Text) - 1 do begin
    GotoXY(Fensterpos.x + 1, Fensterpos.y + 2 + i);
    Write(' ', Text[i], StringOfChar(' ', breite - Length(Text[i]) + 1));
  end;
end;

function TMessageBox.Run: byte;
var
  ch: char;
begin
  cursoroff;
  Paint;
  repeat
    ch := ReadKey;
  until (ch = #13) or (ch = #27);
  Result := byte(ch);
  cursoron;
end;

procedure TMessageBox.SetColor(c: byte);
begin
  col := c;
end;

end.
