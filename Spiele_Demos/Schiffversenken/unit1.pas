unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, Spin, MaskEdit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DrawGrid1: TDrawGrid;
    procedure Button1Click(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure DrawGrid1Selection(Sender: TObject; aCol, aRow: integer);
    procedure FormCreate(Sender: TObject);
  private
    Feld: array of array of record
      Schiff: boolean;
      geklickt: boolean;
    end;
    procedure Schiffe_erzeugen;

  public

  end;

var
  Form1: TForm1;

const
  max: TPoint = (x: 10; y: 10);

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.DrawGrid1DrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
begin

  if (aCol > 0) and (aRow > 0) then begin
    with DrawGrid1 do begin
      if Feld[aCol - 1, aRow - 1].geklickt then begin
        if Feld[aCol - 1, aRow - 1].Schiff then begin
          Canvas.Brush.Color := clRed;
          Canvas.FillRect(aRect);
        end else begin
          Canvas.Brush.Color := clBlue;
          Canvas.FillRect(aRect);
        end;
      end else begin
        if Feld[aCol - 1, aRow - 1].Schiff then begin
          Canvas.Brush.Color := clMaroon;
          Canvas.FillRect(aRect);
        end else begin
          Canvas.Brush.Color := clWhite;
          Canvas.FillRect(aRect);
        end;
      end;
    end;
  end;

  if (aCol = 0) and (aRow > 0) then begin
    DrawGrid1.Canvas.TextOut(aRect.Left, aRect.Top, IntToStr(aRow));
  end;
  if (aCol > 0) and (aRow = 0) then begin
    DrawGrid1.Canvas.TextOut(aRect.Left, aRect.Top, char(aCol + 64));
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Schiffe_erzeugen;
  DrawGrid1.Repaint;
end;

procedure TForm1.DrawGrid1Selection(Sender: TObject; aCol, aRow: integer);
begin
  if (aCol > 0) and (aRow > 0) then begin
    Feld[aCol - 1][aRow - 1].geklickt := True;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  SetLength(Feld, max.x, max.y);
  Schiffe_erzeugen;
  with DrawGrid1 do begin
    ScrollBars := ssNone;
    Left := 0;
    Top := 0;
    ColCount := max.x + 1;
    RowCount := max.y + 1;
    DefaultColWidth := 32;
    DefaultRowHeight := 32;
    Width := DefaultColWidth * ColCount + 2;
    Height := DefaultRowHeight * RowCount + 2;
  end;
end;

procedure TForm1.Schiffe_erzeugen;
var
  x, y, i: integer;
  sp: TPoint;

  function PixelTest(x, y: integer): boolean;
  begin
    if (x < 0) or (y < 0) or (x >= max.x) or (y >= max.y) then begin
      Result := False;
      Exit;
    end;
    Result := Feld[x][y].Schiff;
  end;

  function WasserTest(px, py: integer): boolean;
  var
    x, y: integer;
  begin
    for x := -1 to 1 do begin
      for y := -1 to 1 do begin
        Result := PixelTest(px + x, py + y);
        if Result then begin
          Exit;
        end;
      end;
    end;
  end;

  procedure erzeuge(l: integer);
  var
    i: integer;
    io: boolean;
  begin

    repeat
      io := True;
      if Random(2) = 1 then begin
        sp.x := Random(max.x - (l - 1));
        sp.y := Random(max.y);

        for i := 0 to (l - 1) do begin
          if WasserTest(sp.x + i, sp.y) then begin
            io := False;
          end;
        end;
        if io then begin
          for i := 0 to (l - 1) do begin
            Feld[sp.x + i, sp.y].Schiff := True;
          end;
        end;
      end else begin
        sp.x := Random(max.x);
        sp.y := Random(max.y - (l - 1));

        for i := 0 to (l - 1) do begin
          if WasserTest(sp.x, sp.y + i) then begin
            io := False;
          end;
        end;
        if io then begin
          for i := 0 to (l - 1) do begin
            Feld[sp.x, sp.y + i].Schiff := True;
          end;
        end;
      end;

    until io;
  end;

begin
  for x := 0 to max.x - 1 do begin
    for y := 0 to max.y - 1 do begin
      Feld[x][y].geklickt := False;
      Feld[x][y].Schiff := False;
    end;
  end;

  erzeuge(4);
  for i := 0 to 1 do begin
    erzeuge(3);
  end;
  for i := 0 to 2 do begin
    erzeuge(2);
  end;
  for i := 0 to 3 do begin
    erzeuge(1);
  end;

end;

end.
