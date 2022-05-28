unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

var
  fQuad: array[1..4, 1..2] of real = ((-5, -2), (4, -2), (4, 3), (-3, 3));

{ TForm1 }

procedure Scale(Faktor: real);
var
  x, y: integer;
begin
  for x := 1 to 4 do begin
    for y := 1 to 2 do begin
      fQuad[x, y] := fQuad[x, y] * Faktor;
    end;
  end;
end;

procedure Drehen(Winkel: real);
var
  i: integer;
  x, y: real;

begin
  Winkel := Winkel / 360 * (2 * Pi);
  for i := 1 to 4 do begin
    x := fQuad[i, 1];
    y := fQuad[i, 2];
    fQuad[i, 1] := x * cos(Winkel) - y * sin(Winkel);
    fQuad[i, 2] := x * sin(Winkel) + y * cos(Winkel);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Scale(1.4);
  Paint;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Drehen(30);
  Paint;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Color := 0;
  scale(20);
end;

procedure TForm1.FormPaint(Sender: TObject);

var
  i, x, y: integer;
  K: array[1..4] of record
    x, y: integer;
    end;

begin
  x := ClientWidth div 2;
  y := ClientHeight div 2;
  for i := 1 to 4 do begin
    K[i].x := Round(fQuad[i, 1]) + x;
    K[i].y := Round(fQuad[i, 2]) + y;
  end;
  Canvas.Pen.Color := $FF;
  Canvas.Line(k[1].x, k[1].y, k[2].x, k[2].y);
  Canvas.Pen.Color := $FFFFFF;
  Canvas.Line(k[2].x, k[2].y, k[3].x, k[3].y);
  Canvas.Pen.Color := $FF00;
  Canvas.Line(k[3].x, k[3].y, k[4].x, k[4].y);
  Canvas.Pen.Color := $FF0000;
  Canvas.Line(k[4].x, k[4].y, k[1].x, k[1].y);
end;


end.
