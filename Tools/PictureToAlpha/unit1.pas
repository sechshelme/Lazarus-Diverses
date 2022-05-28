unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, BGRABitmap, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  Bit: TBGRABitmap;
  p: PBGRAPixel;
  w, h: integer;
  i: integer;
  c: TColor;
  DateiName: string;
begin
  if OpenDialog1.Execute then begin
    DateiName := OpenDialog1.FileName;
    Image1.Picture.LoadFromFile(DateiName);

    w := Image1.Picture.Bitmap.Width;
    h := Image1.Picture.Bitmap.Height;

    Bit := TBGRABitmap.Create(w, h);
    //  Bit.Fill($ff);
    p := Bit.Data;

    for i := 0 to Bit.NbPixels - 1 do begin
      c := Image1.Picture.Bitmap.Canvas.Pixels[i mod w, i div w];
      p^.red := c mod $100;
      p^.green := c mod $10000 div $100;
      p^.blue := c mod $1000000 div $10000;
      p^.alpha := 255-(c mod $100);

      Inc(p);
    end;


    //for i := 0 to 1000 do begin
    //  Write(Bit.Bitmap.RawImage.Data[i], ' ');
    //end;


    Bit.SaveToFile(DateiName+'.bmp');
    Bit.Free;
  end;
end;

end.


