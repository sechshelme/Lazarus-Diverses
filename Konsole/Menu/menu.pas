unit Menu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crt, Shares;

type

  { TMenu }

  TMenu = class(TObject)
  private
    menuPos: integer;
    breite: integer;
    Fensterpos: TPoint;
    col1: byte;
    col2: byte;
    Hotcol1: byte;
    Hotcol2: byte;
    Name: array of record
      Text: string;
      HotKey: char;
      HotKeypos: integer;
    end;
  public
    constructor Create(x, y: integer);
    constructor Create;
    procedure Add(s: string);
    procedure Paint;
    procedure SetColor(c1, c2, c3, c4: byte);
    function Run: integer;
    function GetItems: integer;
  end;

implementation

{ TMenu }

constructor TMenu.Create(x, y: integer);
begin
  inherited Create;

  menuPos := 0;
  breite := 0;
  col1 := $70;
  col2 := $2F;
  Hotcol1 := $7E;
  Hotcol2 := $2E;
  Fensterpos.x := x;
  Fensterpos.y := y;
end;

constructor TMenu.Create;
begin
  Create(10, 3);
end;

procedure TMenu.Add(s: string);
var
  p: SizeInt;
begin
  SetLength(Name, Length(Name) + 1);
  p := Pos('&', s);
  if p = 0 then begin
    Name[Length(Name) - 1].HotKey := #0;
    Name[Length(Name) - 1].HotKeypos := 0;
  end else begin
    Delete(s, p, 1);
    Name[Length(Name) - 1].HotKeypos := p;
    Name[Length(Name) - 1].HotKey := UpCase(s[p]);
  end;
  if Length(s) > breite then begin
    breite := Length(s);
  end;
  Name[Length(Name) - 1].Text := s;
end;

procedure TMenu.Paint;
var
  i: integer;
begin
  with Fensterpos do begin
    TextAttr := col1;
    Rahmen(x, y, x + breite + 3, y + Length(Name) + 1, 2);
  end;
  for i := 0 to Length(Name) - 1 do begin
    if i = MenuPos then begin
      TextAttr := col2;
    end else begin
      TextAttr := col1;
    end;
    GotoXY(Fensterpos.x + 1, Fensterpos.y + 1 + i);
    Write(' ', Name[i].Text, StringOfChar(' ', breite - Length(Name[i].Text) + 1));
    if Name[i].HotKeypos > 0 then begin
      if i = MenuPos then begin
        TextAttr := Hotcol2;
      end else begin
        TextAttr := Hotcol1;
      end;
      GotoXY(Fensterpos.x + 1 + Name[i].HotKeypos, WhereY);
      Write(Name[i].Text[Name[i].HotKeypos]);
    end;
  end;
end;

procedure TMenu.SetColor(c1, c2, c3, c4: byte);
begin
  col1 := c1;
  col2 := c2;
  Hotcol1 := c3;
  Hotcol2 := c4;
end;

function TMenu.Run: integer;
var
  Key: char;
  i: integer;
begin
  cursoroff;
  Paint;
  repeat
    Key := ReadKey;
    if Key = #0 then begin
      Key := ReadKey;
      case Key of
        #71: begin
          menuPos := 0;
        end;
        #72: begin
          Dec(menuPos);
          if menuPos < 0 then begin
            menuPos := Length(Name) - 1;
          end;
        end;
        #79: begin
          menuPos := Length(Name) - 1;
        end;
        #80: begin
          Inc(menuPos);
          if menuPos > Length(Name) - 1 then begin
            menuPos := 0;
          end;
        end;
      end;
    end else begin
      for i := 0 to Length(Name) - 1 do begin
        if UpCase(Key) = Name[i].HotKey then begin
          menuPos := i;
          Key := #13;
        end;
      end;
    end;
    Paint;
  until (Key = #13) or (Key = #27);
  if Key = #13 then begin
    Result := menuPos;
  end else begin
    Result := 255;
  end;
  with Fensterpos do begin
    TextAttr := col1;
    Rahmen(x, y, x + breite + 3, y + Length(Name) + 1, 1);
  end;
  cursoron;
end;

function TMenu.GetItems: integer;
begin
  Result := Length(Name) - 1;
end;


end.
