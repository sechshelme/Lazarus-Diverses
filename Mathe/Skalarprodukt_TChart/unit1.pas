unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TASeries, Forms, Controls,
  Graphics, Dialogs, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    Chart1LineSeries3: TLineSeries;
    Chart1LineSeries4: TLineSeries;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function Skalar(x1, y1, x2, y2: single): single;
begin
  Result := ((x1 * x2 + y1 * y2) / (sqrt(x1 * x1 + y1 * y1) * sqrt(x2 * x2 + y2 * y2)));
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  x, y, b: single;
begin
  Chart1LineSeries1.LinePen.Color := clRed;
  Chart1LineSeries2.LinePen.Color := clBlue;
  Chart1LineSeries3.LinePen.Color := clYellow;
  Chart1LineSeries4.LinePen.Color := clGreen;
  for i := 0 to 360 * 2 - 1 do begin
    b := i / 180 * Pi;
    x := sin(b);
    y := cos(b);

    Chart1LineSeries1.AddY(Skalar(x, y, 1.0, 0.0));
    Chart1LineSeries2.AddY(max(Skalar(x, y, 1.0, 0.0), 0.0) + 0.01);
    Chart1LineSeries3.AddY(arccos(Skalar(x, y, -1.0, 0.0)) / Pi + 0.02);
    Chart1LineSeries4.AddY(max(arccos(Skalar(x, y, -1.0, 0.0)) / Pi * 2-1.0,0.0));
  end;
end;

end.
