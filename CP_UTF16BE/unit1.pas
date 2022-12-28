unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
const
  start = 32;
var
  ws: WideString;
  i: integer;
begin
  for i := start to 65536 do begin
    ws := ws + widechar(i);
  end;
  Memo1.Clear;
  memo1.Lines.Add(ws);
end;

end.
