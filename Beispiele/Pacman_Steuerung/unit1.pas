unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLType;

type

  { TForm1 }

  TForm1 = class(TForm)
    ShapePacman: TShape;
    ShapeBall: TShape;
    Timer1: TTimer;
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
  private
    Keys: array[0..$FF] of boolean;
    TopBall: boolean;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  Keys[Key] := True;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  Keys[Key] := False;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if TopBall then begin
    if ShapeBall.Top > 0 then begin
      ShapeBall.Top := ShapeBall.Top - 1;
    end else begin
      TopBall := False;
    end;
  end else begin
    if ShapeBall.Top < ClientHeight - ShapeBall.Height then begin
      ShapeBall.Top := ShapeBall.Top + 1;
    end else begin
      TopBall := True;
    end;
  end;

  if Keys[VK_F] then begin
    Timer1.Interval := 200;
  end else begin
    Timer1.Interval := 10;
  end;

  if Keys[VK_LEFT] then begin
    ShapePacman.Left := ShapePacman.Left - 1;
    if ShapePacman.Left < 0 then begin
      ShapePacman.Left := 0;
    end;
  end;
  if Keys[VK_RIGHT] then begin
    ShapePacman.Left := ShapePacman.Left + 1;
    if ShapePacman.Left > Self.ClientWidth - ShapePacman.Width then begin
      ShapePacman.Left := Self.ClientWidth - ShapePacman.Width;
    end;
  end;
  if Keys[VK_UP] then begin
    ShapePacman.Top := ShapePacman.Top - 1;
    if ShapePacman.Top < 0 then begin
      ShapePacman.Top := 0;
    end;
  end;
  if Keys[VK_DOWN] then begin
    ShapePacman.Top := ShapePacman.Top + 1;
    if ShapePacman.Top > Self.ClientHeight - ShapePacman.Height then begin
      ShapePacman.Top := Self.ClientHeight - ShapePacman.Height;
    end;
  end;
end;

end.
