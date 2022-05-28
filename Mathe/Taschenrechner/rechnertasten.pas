unit RechnerTasten;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type   { TRechnerTasten }

  TRechnerTasten = class(TObject)
    type
    TButtonClick =
    procedure(Sender: TObject; s: string) of object;
  private
    Buttons: array of TButton;
    FColumns: cardinal;
    WinControl: TWinControl;
    FButtonClick: TButtonClick;
    procedure ButtonClick(Sender: TObject);
    procedure SetColumns(AValue: cardinal);
  public
    property OnButtonClick: TButtonClick read FButtonClick write FButtonClick;
    property Columns: cardinal read FColumns write SetColumns;
    constructor Create(AWinControl: TWinControl);
    procedure Add(ACaption: TCaption);
  end;

implementation

{ TRechnerTasten }

procedure TRechnerTasten.ButtonClick(Sender: TObject);
begin
  if Assigned(OnButtonClick) then begin
    OnButtonClick(Sender, TButton(Sender).Caption);
  end;
end;

procedure TRechnerTasten.SetColumns(AValue: cardinal);
var
  i: integer;
begin
  if FColumns = AValue then begin
    Exit;
  end;
  FColumns := AValue;
  for i := 0 to Length(Buttons) - 1 do begin
    with Buttons[i] do begin
      Left := i mod FColumns * 30;
      Top := i div FColumns * 30;
    end;
  end;
end;

constructor TRechnerTasten.Create(AWinControl: TWinControl);
begin
  inherited Create;
  WinControl := AWinControl;
  Columns := 5;
end;

procedure TRechnerTasten.Add(ACaption: TCaption);
var
  l: integer;
begin
  l := Length(Buttons);
  SetLength(Buttons, l + 1);
  Buttons[l] := TButton.Create(WinControl);
  with Buttons[l] do begin
    Parent := WinControl;
    Width := 25;
    Height := 25;
    Parent := WinControl;
    Left := l mod FColumns * 30;
    Top := l div FColumns * 30;
    Caption := ACaption;
    OnClick := @ButtonClick;
  end;
end;

end.
