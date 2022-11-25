unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LMessages, StdCtrls,
  ExtCtrls, LazUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
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
  if TheMessage.Msg = LM_KEYDOWN then begin
    WriteLn('down');
  end;
  if TheMessage.Msg = LM_PAINT then begin
    WriteLn('paint');
    Canvas.Rectangle(10, 10, Width - 10, Height - 10);
  end;
  if TheMessage.Msg = LM_KEYUP then begin
    WriteLn('up');
  end;
  inherited WndProc(TheMessage);
end;

{ TMyForm }

end.
