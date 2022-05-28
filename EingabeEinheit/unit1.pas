unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, EingabeControl, Types, EingabeForm;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure LabelClick(Sender: TObject);
  private
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.LabelClick(Sender: TObject);
var
  f: TForm;
  L: TLabel;
begin
  L := TLabel(Sender);
  EingabeForm1.Wert := L.Caption;
  if EingabeForm1.ShowModal = mrOk then begin
    L.Caption := EingabeForm1.Wert;
  end;
  //EingabeForm.ShowModal;

  //EingabeControl := TEingabeControl.Create(self, 12);
  //with EingabeControl do begin
  //  Parent:=Self;

  //end;

  //EingabeControl.Wert := L.Caption;
  //  if EingabeControl.ShowModal = mrOk then begin
  //    L.Caption := EingabeControl.Wert;
  //  end;
  //  EingabeControl.Free;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  Caption := IntToStr(WheelDelta);
end;

procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin

end;

end.
