unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    LabeledEdit5: TLabeledEdit;
    LabeledEdit6: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
    procedure LabeledEdit1Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.LabeledEdit1Change(Sender: TObject);
var
  x1, y1, z1,
  x2, y2, z2,
  winkel: single;
  e: integer;
begin
  Label1.Caption := 'Error';
  val(LabeledEdit1.Text, x1, e);
  if e <> 0 then begin
    Exit;
  end;
  val(LabeledEdit2.Text, y1, e);
  if e <> 0 then begin
    Exit;
  end;
  val(LabeledEdit3.Text, z1, e);
  if e <> 0 then begin
    Exit;
  end;
  val(LabeledEdit4.Text, x2, e);
  if e <> 0 then begin
    Exit;
  end;
  val(LabeledEdit5.Text, y2, e);
  if e <> 0 then begin
    Exit;
  end;
  val(LabeledEdit6.Text, z2, e);
  if e <> 0 then begin
    Exit;
  end;

  winkel := arccos((x1 * x2 + y1 * y2 + z1 * z2) / (sqrt(x1 * x1 + y1 * y1 + z1 * z1) * sqrt(x2 * x2 + y2 * y2 + z2 * z2)));
  winkel := winkel / pi * 180;

  Label1.Caption := FloatToStr(winkel);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LabeledEdit1Change(Sender);
end;

end.
