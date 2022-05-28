unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormClick(Sender: TObject);
const
  faktor = 3000;
var
  w, h, i: integer;
  r1, r2: single;

  function rnd: single;
  begin
    Result := random * 0.001 + 0.00015;
  end;

begin
  Canvas.Clear;
  r1 := rnd;
  r2 := rnd;
  Caption := FloatToStr(r1) + '  ' + FloatToStr(r2);
  w := ClientWidth div 2;
  h := ClientHeight div 2;
  for i := 1 to ClientHeight * faktor do begin
    Canvas.LineTo(
      round(sin(i * r1) * ((w - i) / faktor / 2)) + w,
      round(sin(i * r2) * ((h - i) / faktor / 2)) + h);
    Application.ProcessMessages;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
end;

end.
