unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public
    procedure Info(Bitmap: TBitmap; SLInfo: TStrings);

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Info(Bitmap: TBitmap; SLInfo: TStrings);
begin
  with Bitmap.RawImage.Description do begin
    SLInfo.Add(
      '  BitsPerPixel:' + IntToStr(BitsPerPixel) + LineEnding +
      '  RedPrec:     ' + IntToStr(RedPrec) + LineEnding +
      '  RedShift:    ' + IntToStr(RedShift) + LineEnding + LineEnding +
      '  GreenPrec:   ' + IntToStr(GreenPrec) + LineEnding +
      '  GreenShift:  ' + IntToStr(GreenShift) + LineEnding + LineEnding +
      '  BluePrec:    ' + IntToStr(BluePrec) + LineEnding +
      '  BlueShift:   ' + IntToStr(BlueShift) + LineEnding + LineEnding + LineEnding +
      '  AlphaPrec:   ' + IntToStr(AlphaPrec) + LineEnding +
      '  AlphaShift:  ' + IntToStr(AlphaShift) + LineEnding + LineEnding);
  end;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Info(Image1.Picture.Bitmap, Memo1.Lines);
  Image1.Picture.LoadFromFile('project1.ico');
  Info(Image1.Picture.Bitmap, Memo1.Lines);
  Image1.Picture.Bitmap.PixelFormat := pf32bit;
  Info(Image1.Picture.Bitmap, Memo1.Lines);
  Image1.Picture.Bitmap.PixelFormat := pf24bit;
  Info(Image1.Picture.Bitmap, Memo1.Lines);
end;


end.
