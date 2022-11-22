unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TTest }

  TTest = class(TObject)
  private
    FFont: TFont;
    procedure SetFont(AValue: TFont);
  public
    property Font: TFont read FFont write SetFont;
    constructor Create;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Test: TTest;

  public

  end;

var
  Form1: TForm1;


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Test := TTest.Create;
  //  Test.Font.Height:=50;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Test.Font.Height := 50;
  WriteLn(Test.Font.Height);
end;

{ TTest }

procedure TTest.SetFont(AValue: TFont);
begin
  WriteLn('font');

  if FFont = AValue then begin
    Exit;
  end;
  FFont := AValue;
end;

constructor TTest.Create;
begin
  Font := TFont.Create;
end;




end.
