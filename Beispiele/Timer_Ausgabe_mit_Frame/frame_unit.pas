unit Frame_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { TFrame1 }

  TFrame1 = class(TFrame)
    Button1, Button2, Button3: TButton;
    Label1: TLabel;
    Timer1: TTimer;
    Panel1: TPanel;
    procedure ButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    TimeAlt, Start: TDateTime;
  end;

implementation

{$R *.lfm}

{ TFrame1 }

procedure TFrame1.ButtonClick(Sender: TObject);
begin
  case TButton(Sender).Tag of
    0: begin
      if Button3.Enabled then begin
        Start := Now - TimeAlt;
      end else begin
        Start := Now;
      end;
      Button1.Enabled := False;
      Button2.Enabled := True;
      Button3.Enabled := True;
      Timer1.Enabled := True;
    end;
    1: begin
      Button1.Enabled := True;
      Button2.Enabled := False;
      Timer1.Enabled := False;
      TimeAlt := Now - Start;
    end;
    2: begin
      Button1.Enabled := True;
      Button2.Enabled := False;
      Button3.Enabled := False;
      Timer1.Enabled := False;
      Label1.Caption := '--:--:--';
    end;
  end;
end;

procedure TFrame1.Timer1Timer(Sender: TObject);
begin
  Label1.Caption := TimeToStr(Now - Start);
end;

end.

