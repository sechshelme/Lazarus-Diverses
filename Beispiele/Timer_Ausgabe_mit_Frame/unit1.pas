unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Frame_unit;

type

  { TForm1 }

  TForm1 = class(TForm)
    ScrollBox1: TScrollBox;
    procedure FormCreate(Sender: TObject);
  public
    Frame: array of TFrame1;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  SetLength(Frame, 10);
  for i := 0 to Length(Frame) - 1 do begin
    Frame[i] := TFrame1.Create(ScrollBox1);
    with Frame[i] do begin
      if i mod 2 = 1 then begin
        Color := clSilver;
      end else begin
        Color := clGray;
      end;
      Name := 'MyFrame' + IntToStr(i);
      Parent := ScrollBox1;
      Top := i * 40;
      Height := 40;
      Width := ScrollBox1.ClientWidth;
    end;
  end;
end;

end.
