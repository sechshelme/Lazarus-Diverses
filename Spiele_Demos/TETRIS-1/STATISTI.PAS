unit statisti;

interface

uses
   Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TStatistikBox = class(TForm)
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private-Deklarationen }
  public
    Anz : Array[0..30] of Integer;
    AnzLabel : Array[0..30] of TLabel;
  end;

var
  StatistikBox: TStatistikBox;

implementation

uses
  Hauptfor;

{$R *.lfm}

procedure TStatistikBox.FormPaint(Sender: TObject);
var
  i, x1, y1, x , y : Integer;
begin
  with Canvas do for i := 0 to anzKloetze - 1 do begin
    if i in [0..6] then begin
      y1 := i * 50 - 15;
      x1 := 0;
    end else if i in[7..13] then begin
      y1 := i * 50 - 15 - 350;
      x1 := 120;
    end else begin
      y1 := i * 50 - 15 - 700;
      x1 := 240;
    end;
    Pen.Color := KlotzColor[i + 1];
    Brush.Color := KlotzColor[i + 1];
    for x := 0 to 3 do begin
      for y := 0 to 3 do begin
        if Kloetze[i, x, y] > 0 then begin
          Rectangle(x * 16 + 1 + x1, y * 16 + 1 + y1, x * 16 + 14 + x1, y * 16 + 14 + y1);
        end;
      end;
    end;
    AnzLabel[i].Caption := IntToStr(Anz[i]);
  end;
end;

procedure TStatistikBox.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to 20 do begin
    AnzLabel[i] := TLabel.Create(Self);
    with AnzLabel[i] do begin
      Parent := Self;
      if i in [0..6] then begin
        Left := 100;
        Top := i * 50 + 10;
      end else if i in[7..13] then begin
        Left := 220;
        Top := i * 50 - 340;
      end else begin
        Left := 340;
        Top := i * 50 - 690;
      end;
      Caption := '0';
      Font.Color := $FFFFFF;
      Alignment := taRightJustify;
    end;
  end;
  ClientWidth := 380;
  ClientHeight := 350;
end;

procedure TStatistikBox.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  HauptFormular.FormKeyDown(Sender, Key, Shift);
end;

end.
