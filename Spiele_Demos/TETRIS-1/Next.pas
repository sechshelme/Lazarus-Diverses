unit Next;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, {GradForm,}
  Digital;

type
  TNextBox = class(TForm)
    procedure FormPaint(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  NextBox: TNextBox;

implementation

{$R *.lfm}

uses
  Hauptfor;

procedure TNextBox.FormPaint(Sender: TObject);
var
  col, x, y : Integer;
begin
  with AktKlotz, Canvas do for x := 0 to 3 do for y := 0 to 3 do begin
    col := Neu[x, y];
    if col > 0 then begin
      Brush.Color := KlotzColor[col];
      Pen.Color := KlotzColor[col];
      Rectangle(x * 24 + 2, y * 24 + 2, x * 24 + 22, y * 24 + 22);
    end;
  end;
end;

procedure TNextBox.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  HauptFormular.FormKeyDown(Sender, Key, Shift);
end;

end.
