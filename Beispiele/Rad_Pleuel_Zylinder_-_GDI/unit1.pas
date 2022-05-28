unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TRadPanel }

  TRadPanel = class(TPanel)
  private
    FWinkel: single;
    procedure SetWinkel(AValue: single);
  public
    property Winkel: single read FWinkel write SetWinkel;
    constructor Create(TheOwner: TComponent); override;
    procedure RadPanelPaint(Sender: TObject);
  end;


  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    Schritt: single;
  public
    RadPanel: TRadPanel;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TRadPanel }

procedure TRadPanel.SetWinkel(AValue: single);
begin
  if FWinkel = AValue then begin
    Exit;
  end;
  FWinkel := AValue;
end;

constructor TRadPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnPaint := @RadPanelPaint;
end;

procedure TRadPanel.RadPanelPaint(Sender: TObject);
const
  Speichen = 15;
var
  RadiusRad,
  i: integer;
  l: integer;
  p: integer;
  r: single;
  radz, z: TPoint;
begin

  l := Width div 3;
  radz.x := Width div 5;
  radz.y := Height div 2;
  RadiusRad := radz.y div 3 * 2;
  r := RadiusRad / 2;

  Canvas.Pen.Width := radz.y div 15;

  Canvas.EllipseC(radz.x, radz.y, RadiusRad, RadiusRad);
  for i := 0 to Speichen - 1 do begin
    Canvas.Line(radz.x, radz.y,
      radz.x + round(cos(Pi * 2 / Speichen * i + FWinkel) * RadiusRad),
      radz.y + round(sin(Pi * 2 / Speichen * i + FWinkel) * RadiusRad));
  end;
  z.y := round(sin(FWinkel) * r);
  z.x := round(cos(FWinkel) * r);
  Canvas.EllipseC(radz.x + z.x, radz.y + z.y, 10, 10);

    p := round(sqrt(sqr(l) - sqr(z.y)) + z.x);

  Canvas.EllipseC(radz.x + p, radz.y, 10, 10);
  Canvas.Line(radz.x + z.x, radz.y + z.y, radz.x + p, radz.y);

  Canvas.Rectangle(radz.x + p + 180, radz.y - 50, radz.x + p + 220, radz.y + 50);
  Canvas.Line(radz.x + p, radz.y, radz.x + p + 200, radz.y);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Schritt := 0.03;
  RadPanel := TRadPanel.Create(Self);
  RadPanel.Align := alClient;
  RadPanel.Parent := Self;
  DoubleBuffered := True;
  Button1.Parent := RadPanel;
  Button2.Parent := RadPanel;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Schritt /= 1.1;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Schritt *= 1.1;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  RadPanel.Winkel := RadPanel.Winkel + Schritt;
  if RadPanel.Winkel > 2 * Pi then begin
    RadPanel.Winkel := RadPanel.Winkel - 2 * Pi;
  end;
  Repaint;
//  Caption := FloatToStr(RadPanel.Winkel);
end;

end.
