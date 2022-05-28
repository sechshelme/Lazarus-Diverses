unit Tisch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, Controls;

type

  { TTisch }

  TTisch = class(TPanel)
  private
    FAnzahl: integer;
    FWinkel: single;
    Posalt: integer;
    procedure SetAnzahl(AValue: integer);
    procedure SetWinkel(AValue: single);
  public
    property Anzahl: integer read FAnzahl write SetAnzahl;
    property Winkel: single read FWinkel write SetWinkel;
    constructor Create(Component: TComponent); override;
    procedure SetPos(APos: integer);
    procedure TischPaint(Sender: TObject);
  end;

implementation

{ TTisch }

procedure TTisch.SetAnzahl(AValue: integer);
begin
  if FAnzahl = AValue then begin
    Exit;
  end;
  FAnzahl := AValue;
  if FAnzahl < 2 then begin
    FAnzahl := 2;
  end;
  Repaint;
end;

procedure TTisch.SetWinkel(AValue: single);
begin
  if FWinkel = AValue then begin
    Exit;
  end;
  FWinkel:=AValue;
  if FWinkel < 0.0 then begin
    FWinkel += 2 * Pi;
  end else if FWinkel > 2 * Pi then begin
    FWinkel -= 2 * Pi;
  end;
end;

constructor TTisch.Create(Component: TComponent);
begin
  inherited Create(Component);
  Anzahl := 18;
  Winkel := 0.0;
  Posalt := 0;
  OnPaint := @TischPaint;
  Color := clBlack;
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

procedure TTisch.SetPos(APos: integer);
begin
  Posalt := APos;
end;

procedure TTisch.TischPaint(Sender: TObject);
var
  i, RadiusX, RadiusY: integer;
  col: TColor;
  r: single;

  function getRadius(r: single; anz: integer): single;
  begin
    Result := r * (sin(pi / anz)) / (1 + sin(pi / anz));
  end;

begin
  r := getRadius(Width / 2, Anzahl);
  RadiusX := round(r);
  RadiusY := round(r * (Height / Width));

  for i := 0 to Anzahl - 1 do begin
    case i mod 3 of
      0: begin
        Col := clRed;
      end;
      1: begin
        Col := clYellow;
      end;
      2: begin
        Col := clGreen;
      end;
    end;
    if Posalt <> i then begin
      col := col div 5;
    end;
    Canvas.Brush.Color := col;
    Canvas.Ellipse(
      round(Width / 2 - RadiusX + sin(2 * Pi / Anzahl * i + Winkel) * (Width / 2 - RadiusX)),
      round(Height / 2 - RadiusY + cos(2 * Pi / Anzahl * i + Winkel) * (Height / 2 - RadiusY)),

      round(Width / 2 + RadiusX + sin(2 * Pi / Anzahl * i + Winkel) * (Width / 2 - RadiusX)),
      round(Height / 2 + RadiusY + cos(2 * Pi / Anzahl * i + Winkel) * (Height / 2 - RadiusY)));
  end;
end;


end.
