unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormClick(Sender: TObject);
var
  z:Double;
  i: Integer;
begin
  Canvas.Pen.Color:=Random($FFFFFF);
  z:=now;
  for i:=1 to 1000000 do begin
    Canvas.Line(0,0,500,500);
  end;
  WriteLn(Now-z:20:10);
end;

end.

