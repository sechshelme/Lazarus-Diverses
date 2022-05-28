unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLType, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
  private
    Keys: array[0..$FF] of boolean;
    BallPos: record
      x, y, Schritt, sx, sy: integer;
    end;
  const
    rot: integer = 0;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FillChar(Keys, SizeOf(Keys), 0);
  Ballpos.x := ClientWidth div 2;
  Ballpos.y := ClientHeight div 2;
  Ballpos.Schritt := 3;
  Ballpos.sx := Ballpos.Schritt;
  Ballpos.sy := Ballpos.Schritt;
  DoubleBuffered := True;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  k: byte;
begin
  k := Key;
  Keys[k] := True;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
var
  k: byte;
begin
  k := Key;
  Keys[k] := False;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  p: integer;
const
  ss = 5;
begin
  p := Shape1.Top;
  if Keys[VK_A] then begin
    p -= ss;
  end;
  if Keys[VK_Y] then begin
    p += ss;
  end;
  if p < 10 then begin
    p := 10;
  end else if p > ClientHeight - Shape1.Height - 10 then begin
    p := ClientHeight - Shape1.Height - 10;
  end;
  Shape1.Top := p;

  p := Shape2.Top;
  if Keys[VK_K] then begin
    p -= ss;
  end;
  if Keys[VK_M] then begin
    p += ss;
  end;
  if p < 10 then begin
    p := 10;
  end else if p > ClientHeight - Shape2.Height - 10 then begin
    p := ClientHeight - Shape2.Height - 10;
  end;
  Shape2.Top := p;

  Shape2.Left := ClientWidth - Shape2.Width - 10;

  Ballpos.x += Ballpos.sx;
  Ballpos.y += Ballpos.sy;
  if Ballpos.x + Shape3.Width >= ClientWidth then begin
    Ballpos.sx := -Ballpos.Schritt;
    rot := 10;
  end;
  if Ballpos.x <= 0 then begin
    Ballpos.sx := Ballpos.Schritt;
    rot := 10;
  end;
  if rot > 0 then begin
    Dec(rot);
  end;
  if rot > 0 then begin
    Shape3.Brush.Color := $FF;
  end else begin
    Shape3.Brush.Color := $FFFFFF;
  end;


  if Ballpos.y + Shape3.Height >= ClientHeight then begin
    Ballpos.sy := -Ballpos.Schritt;
  end;
  if Ballpos.y <= 0 then begin
    Ballpos.sy := Ballpos.Schritt;
  end;


  if (Ballpos.x <= Shape1.Left + Shape1.Width) and (Ballpos.y >= Shape1.Top) and (Ballpos.y <= Shape1.Top + Shape1.Height) then begin
    Ballpos.sx := Ballpos.Schritt;
  end;

  if (Ballpos.x >= Shape2.Left - Shape3.Width) and (Ballpos.y >= Shape2.Top) and (Ballpos.y <= Shape2.Top + Shape2.Height) then begin
    Ballpos.sx := -Ballpos.Schritt;
  end;
  Caption := IntToStr(Ballpos.y);

  Shape3.Left := BallPos.x;
  Shape3.Top := BallPos.y;
end;

end.
