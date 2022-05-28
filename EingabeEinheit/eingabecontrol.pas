unit EingabeControl;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Types;

type

  TOnChange = procedure(Sender: TObject) of object;

  { TEingabeEinheit }

  TEingabeEinheit = class(TPanel)
  private
    BtnUp, BtnDown: TBitBtn;
    FWert: char;

    FOnChange: TOnChange;

    procedure EingabeEinheitMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure SetWert(AValue: char);
    procedure BtnClick(Sender: TObject);
    procedure EinageEinheitPaint(Sender: TObject);
    procedure EinageEinheitResize(Sender: TObject);
    procedure Up;
    procedure Down;
  public
    property Wert: char read FWert write SetWert;
    constructor Create(TheOwner: TComponent); override;
  published
    property OnChange: TOnChange read FOnChange write FOnChange;
  end;


  { TEingabeControl }


  TEingabeControl = class(TWinControl)
  private
    anzEinheiten: integer;
    EingabeEinheit: array of TEingabeEinheit;

    FOnChange: TOnChange;

    procedure EingabeEinheitChange(Sender: TObject);
    function GetWert: string;
    procedure SetWert(AValue: string);
  public
    property Wert: string read GetWert write SetWert;
    constructor Create(TheOwner: TComponent; AanzEinheiten: integer = 4);
  published
    property OnChange: TOnChange read FOnChange write FOnChange;
  end;


implementation

{ TEingabeEinheit }

procedure TEingabeEinheit.EinageEinheitResize(Sender: TObject);
var
  size: integer;
begin
  size := Width - 6;

  with BtnUp do begin
    Left := 3;
    Top := 3;
    Width := size;
    Height := size;
    Parent := Self;
  end;

  with BtnDown do begin
    Left := 3;
    Top := Self.Height - size - 3;
    Width := size;
    Height := size;
    Parent := Self;
  end;
end;

procedure TEingabeEinheit.Up;
begin
  Inc(FWert);
  if FWert > '9' then begin
    FWert := '0';
  end;
  if Assigned(OnChange) then begin
    OnChange(Self);
  end;
end;

procedure TEingabeEinheit.Down;
begin
  Dec(FWert);
  if FWert < '0' then begin
    FWert := '9';
  end;
  if Assigned(OnChange) then begin
    OnChange(Self);
  end;
end;

procedure TEingabeEinheit.BtnClick(Sender: TObject);
begin
  if Sender = BtnUp then begin
    Up;
  end;
  if Sender = BtnDown then begin
    Down;
  end;
  Invalidate;
end;

procedure TEingabeEinheit.EingabeEinheitMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  if WheelDelta < 0 then begin
    Up;
  end;
  if WheelDelta > 0 then begin
    Down;
  end;
  Invalidate;
end;

procedure TEingabeEinheit.EinageEinheitPaint(Sender: TObject);
var
  s: TSize;
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Height := 76;
  s := Canvas.TextExtent(Wert);
  Canvas.TextOut((Width - s.cx) div 2, (Height - s.cy) div 2, Wert);
end;

procedure TEingabeEinheit.SetWert(AValue: char);
begin
  FWert := AValue;
  Invalidate;
end;

constructor TEingabeEinheit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  BtnUp := TBitBtn.Create(Self);
  BtnUp.Glyph.LoadFromFile('up.bmp');
  BtnUp.OnClick := @BtnClick;

  BtnDown := TBitBtn.Create(Self);
  BtnDown.Glyph.LoadFromFile('down.bmp');
  BtnDown.OnClick := @BtnClick;

  OnResize := @EinageEinheitResize;
  OnPaint := @EinageEinheitPaint;
  OnMouseWheel := @EingabeEinheitMouseWheel;
end;

{ TEingabeControl }

function TEingabeControl.GetWert: string;
var
  i: integer;
begin
  SetLength(Result, anzEinheiten);
  for i := 0 to anzEinheiten - 1 do begin
    Result[i + 1] := EingabeEinheit[i].Wert;
  end;
end;

procedure TEingabeControl.EingabeEinheitChange(Sender: TObject);
var
  s: string;
  i: integer;
begin
  SetLength(s, anzEinheiten);
  for i := 0 to anzEinheiten - 1 do begin
    s[i + 1] := EingabeEinheit[i].Wert;
  end;
  Wert:=s;
  OnChange(Self);
end;

procedure TEingabeControl.SetWert(AValue: string);
var
  l, l2, i: integer;
begin
  for i := 0 to anzEinheiten - 1 do begin
    EingabeEinheit[i].Wert := '0';
  end;
  if Length(AValue) < anzEinheiten then begin
    l := Length(AValue);
  end else begin
    l := anzEinheiten;
  end;
  l2 := anzEinheiten - Length(AValue);
  if l2 < 0 then begin
    l2 := 0;
  end;
  for i := 0 to l - 1 do begin
    EingabeEinheit[i + l2].Wert := AValue[i + 1];
  end;
end;

constructor TEingabeControl.Create(TheOwner: TComponent; AanzEinheiten: integer);
const
  w = 50;
var
  i: integer;
begin
  inherited Create(TheOwner);
  anzEinheiten := AanzEinheiten;
  Height := 200;
  Width := w * anzEinheiten + 10;
  Constraints.MinWidth := 95 + 80 + 10;

  SetLength(EingabeEinheit, anzEinheiten);
  for i := 0 to anzEinheiten - 1 do begin
    EingabeEinheit[i] := TEingabeEinheit.Create(Self);
    with EingabeEinheit[i] do begin
      Top := 0;
      Left := w * i;
      Width := w;
      Height := Self.Height;
      Parent := Self;
      Wert := '0';
      OnChange := @EingabeEinheitChange;
    end;
  end;
end;

end.
