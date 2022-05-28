unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Types,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Image1: TImage;
    Memo1: TMemo;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
//{$R test.rc}

//{$R test.glsl}

//{$R "test.glsl" Type="RCDATA" ResourceName="TEST"}


{ TForm1 }

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  rs: TResourceStream;
begin
  rs := TResourceStream.Create(HINSTANCE, 'test', RT_RCDATA);
  try
    Memo1.Lines.LoadFromStream(rs);
  finally
    rs.Free;
  end;

   rs := TResourceStream.Create(HINSTANCE, '40', RT_RCDATA);
  try
    Image1.Picture.LoadFromStream(rs);
  finally
    rs.Free;
  end;
end;

end.
