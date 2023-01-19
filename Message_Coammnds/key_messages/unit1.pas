unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LMessages, StdCtrls,
  ExtCtrls, LazUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Panel1: TPanel;
  protected
    procedure WndProc(var TheMessage: TLMessage); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.WndProc(var TheMessage: TLMessage);
begin
  if TheMessage.Msg = LM_LBUTTONDOWN then begin
    WriteLn('Btn1 down');
  end;
  if TheMessage.Msg = LM_LBUTTONUP then begin
    WriteLn('Btn1 up');
  end;
  if TheMessage.Msg = LM_KEYDOWN then begin
    WriteLn('down');
  end;
  if TheMessage.Msg = LM_PAINT then begin
    WriteLn('paint');
    Canvas.Rectangle(20, 20, Width - 20, Height - 20);
  end;
  if TheMessage.Msg = LM_KEYUP then begin
    WriteLn('up');
  end;
  inherited WndProc(TheMessage);
end;

{ TMyForm }

end.
