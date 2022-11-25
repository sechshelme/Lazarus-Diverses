unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LMessages;

type

  { TForm1 }

  TForm1 = class(TForm)
  private
    procedure LMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
    procedure LMKeyUp(var Message: TLMKeyUp); message LM_KEYUP;
    procedure LMPaint(var Message: TLMPaint); message LM_PAINT;

    procedure Paint; override;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.LMKeyDown(var Message: TLMKeyDown);
begin
  WriteLn('down: ', Message.KeyData.ToHexString);
end;

procedure TForm1.LMKeyUp(var Message: TLMKeyUp);
begin
  WriteLn('Up:   ', Message.KeyData.ToHexString);
end;

procedure TForm1.LMPaint(var Message: TLMPaint);
begin
  WriteLn(  Message.PaintStruct^.rcPaint.Width);
  WriteLn('paint');
end;

procedure TForm1.Paint;
begin
  inherited Paint;
end;

end.
