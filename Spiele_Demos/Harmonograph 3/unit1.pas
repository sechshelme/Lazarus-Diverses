unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure updateXY;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;


var
  d, c, p, q, rk, A, B, u, v, Rg, Sg, f, g, h: single;
  alpha, beta, gamma,
  scale: single;
  sk: integer;

const
  x: single = 0.0;
  y: single = 0.0;
  t: single = 0.0;
  dt = 0.01;

implementation

{$R *.lfm}

{ TForm1 }

function toRadians(degrees: single): single;
begin
  Result := degrees / 180.0 * Pi;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  d := 900;
  c := 800;
  p := 900;
  q := 700;
  rk := 300;
  A := toRadians(10);
  B := toRadians(21);
  u := 0;
  v := 0;
  Rg := 0.002;
  Sg := 0.001;
  f := 0.300;
  g := 0.304;
  h := 0.0008;

  sk := 100000;
  scale := min(ClientWidth * 0.9 / 2.0 / rk, ClientHeight * 0.9 / 2.0 / rk);
end;

procedure TForm1.FormShow(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to sk do begin
    Chart1LineSeries1.AddXY(x,y);
    t := t + dt;
    updateXY;
  end;
end;


procedure TForm1.updateXY;
var
  xa, ya, xb, yb: single;
begin
  alpha := A * sin(2.0 * PI * (f * t + u)) * exp(-Rg * t);
  beta := B * sin(2.0 * PI * (g * t + v)) * exp(-Sg * t);
  gamma := 2.0 * PI * h * t;

  xa := p * cos(alpha) + q * sin(alpha) - d;
  ya := q * cos(alpha) - p * sin(alpha);
  xb := xa * cos(beta) - ya * sin(beta);
  yb := ya * cos(beta) + xa * sin(beta) - c;
  x := xb * cos(gamma) - yb * sin(gamma);
  y := yb * cos(gamma) + xb * sin(gamma);


  x:=xb;y:=yb;
               //     x:=alpha;y:=beta;
end;

end.
