unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, GraphType, Dialogs, ExtCtrls,
  Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Image1: TImage;
    Image2: TImage;
    procedure BitBtn1Click(Sender: TObject);
  private

  public

  end;

  { T8BitRawImage }

  T8BitRawImage = object(TRawImage)
    procedure PutPixel(x, y: integer; col: byte);
  end;

  { TMyRawImageDescription }

  TMyRawImageDescription = object(TRawImageDescription)
    procedure Init_BPP8(AWidth, AHeight: integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  Red = %00000111;
  Green = %00111000;
  Blue = %11000000;

{ T8BitRawImage }


procedure T8BitRawImage.PutPixel(x, y: integer; col: byte);
var
  p: PByte;
  ofs: UInt32;
begin
  p := Data;
  ofs := x + y * Description.Width;
  if ofs > DataSize then begin
    Exit;
  end;
  Inc(p, ofs);
  p^ := col;
end;

{ TMyRawImageDescription }

procedure TMyRawImageDescription.Init_BPP8(AWidth, AHeight: integer);
begin
  FillChar(Self, SizeOf(Self), 0);

  Format := ricfRGBA;
  Depth := 8; // used bits per pixel
  Width := AWidth;
  Height := AHeight;
  BitOrder := riboBitsInOrder;
  ByteOrder := riboLSBFirst;
  LineOrder := riloTopToBottom;
  BitsPerPixel := 8; // bits per pixel. can be greater than Depth.
  LineEnd := rileDWordBoundary;
  RedPrec := 3;
  RedShift := 0;
  GreenPrec := 3;
  GreenShift := 3;
  BluePrec := 2;
  BlueShift := 6;
end;

{ TForm1 }

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  raw: T8BitRawImage;
  des: TMyRawImageDescription;

begin
  des.Init_BPP8(16, 16);

  raw.Init;
  raw.Description := des;
  raw.CreateData(True);

  raw.PutPixel(0, 0, Red);
  raw.PutPixel(1, 1, Green);
  raw.PutPixel(2, 2, Blue);
  raw.PutPixel(6, 2, Red or Green);
  raw.PutPixel(6, 3, Green or Blue);
  raw.PutPixel(6, 4, Red or Blue);
  raw.PutPixel(6, 5, Red or Green or Blue);


  WriteLn('raw: ', raw.DataSize);

  Image2.Picture.Bitmap.LoadFromRawImage(raw, True);

  WriteLn('vor Pixels: ', Image2.Picture.Bitmap.RawImage.DataSize);
  WriteLn(Image2.Picture.Bitmap.PixelFormat);
  Image2.Picture.SaveToFile('test.png');
  Image2.Picture.Bitmap.Canvas.Pixels[4, 4] := clRed;
  WriteLn('nach Pixels :', Image2.Picture.Bitmap.RawImage.DataSize);
  WriteLn(Image2.Picture.Bitmap.PixelFormat);

  Image1.Picture.LoadFromFile('test.png');
  WriteLn('laden: ', Image1.Picture.Bitmap.PixelFormat);


  Caption := IntToStr(Image2.Picture.Bitmap.RawImage.Description.BitsPerPixel);
end;

end.
