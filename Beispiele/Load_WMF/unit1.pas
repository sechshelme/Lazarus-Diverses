unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  // Komponente xxx/lazarus/components/fpvectorial installieren.
  fpvectorial, wmfvectorialreader;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  page: TvPage;
  FVec: TvVectorialDocument;
begin
  FVec := TvVectorialDocument.Create;
  FVec.ReadFromFile('test.wmf');
//  FVec.GuessDocumentSize();
  page := FVec.GetPage(0);

  Image1.Picture.Bitmap.Width := round(page.Width);
  Image1.Picture.Bitmap.Height := round(page.Height);

  Image1.Picture.Bitmap.Canvas.Brush.Color := clGreen;
  Image1.Picture.Bitmap.Canvas.FillRect(0, 0, Image1.Picture.Bitmap.Width,
    Image1.Picture.Bitmap.Height);

  page.Render(Image1.Picture.Bitmap.Canvas, 0, 0, 1, 1);
  FVec.Free;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

