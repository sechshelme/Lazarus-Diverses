unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, fpexprpars;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
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
var
  fp: TFPExpressionParser;
  res: TFPExpressionResult;
begin
  fp := TFPExpressionParser.Create(self);
  fp.BuiltIns := [bcMath];
  fp.Identifiers.AddFloatVariable('x', 10.0);
  fp.Identifiers.AddFloatVariable('y', 2.0);

  fp.Expression := 'x + y';
  res := fp.Evaluate;

  Caption := FloatToStr(ArgToFloat(res));

end;

end.

