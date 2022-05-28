unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,oglVector, oglMatrix, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormPaint(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation


var
  p1, p2, p3, p4, sp: TVector2f;

{$R *.lfm}

{ TForm1 }



function getSchnitt(p1, p2, p3, p4: TVector2f): TVector2f;
var
  T0, T1: TVector2f;
begin
  if p1.x = p2.x then begin
    T0 := p3;
    T1 := p4;
    Result.x := p1.x;
  end else begin
    T0 := p1;
    T1 := p2;
    if p3.x = p4.x then begin
      Result.x := p3.x;
    end else begin
      Result.x := ((p3.y - p1.y) + (p2.y - p1.y) / (p2.x - p1.x) * p1.x - (p4.y - p3.y) / (p4.x - p3.x) * p3.x) /
        ((p2.y - p1.y) / (p2.x - p1.x) - (p4.y - p3.y) / (p4.x - p3.x));
    end;
  end;

  Result.y := ((T1.y - T0.y) / (T1.x - T0.x)) * (Result.x - T0.x) + T0.y;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  p1 := vec2(270, 50);
  p2 := vec2(10, 60);
  p3 := vec2(170, 10);
  p4 := vec2(170, 110);
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
const
  p: integer = 1;
begin
  Inc(p);
  if p > 4 then begin
    p := 1;
  end;
  case p of
    1: begin
      p1.x := X;
      p1.y := Y;
    end;
    2: begin
      p2.x := X;
      p2.y := Y;
    end;
    3: begin
      p3.x := X;
      p3.y := Y;
    end;
    4: begin
      p4.x := X;
      p4.y := Y;
    end;
  end;
  Invalidate;
end;

procedure TForm1.FormPaint(Sender: TObject);

  procedure DrawLine(v0, v1: TVector2f);
  begin
    with Canvas do begin
      MoveTo(round(v0.x), round(v0.y));
      LineTo(round(v1.x), round(v1.y));
    end;
  end;

  procedure DrawPoint(v: TVector2f);
  const
    d = 4;
  begin
    with Canvas do begin
      Ellipse(round(v.x - d), round(v.y - d), round(v.x + d), round(v.y) + d);
    end;
  end;

begin
  DrawLine(p1, p2);
  DrawLine(p3, p4);


  sp := getSchnitt(p1, p2, p3, p4);

  Caption := 'X:' + FloatToStr(sp.x) + '  Y:' + FloatToStr(sp.y);
  //  Caption := 'X:' + IntToStr(round(sp.x)) + '  Y:' + IntToStr(maxSmallint);

  DrawPoint(sp);
end;

end.
