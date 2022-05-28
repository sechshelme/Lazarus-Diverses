unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, BMPcomn;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

type
  TDateiKopf = packed record
    bfType: UInt16;
    bfSize: UInt32;
    bfReserved: UInt32;
    bfOffBits: UInt32;
  end;

  TInfoBlock = packed record
    biSize: UInt32;
    biWidth: Int32;
    biHeight: Int32;
    biPlanes: UInt16;
    biBitCount: UInt16;
    biCompression: UInt32;
    biSizeImage: UInt32;
    biXPelsPerMeter: Int32;
    biYPelsPerMeter: Int32;
    biClrUsed: UInt32;
    biClrImportant: UInt32
  end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  dk: TDateiKopf;
  ib: TInfoBlock;
  fs: TFileStream;
const
  Textur32: packed array[0..1, 0..1, 0..3] of byte = ((($FF, $00, $00, $FF), ($00, $FF, $00, $FF)), (($00, $00, $FF, $FF), ($FF, $00, $00, $FF)));
begin
  with dk do begin
    bfType := byte('B') + byte('M') shl 8;
    bfSize := SizeOf(TDateiKopf) + SizeOf(TInfoBlock) + SizeOf(Textur32);
    bfReserved := 0;
    bfOffBits := 54;
  end;

  with ib do begin
    biSize := 40;
    biWidth := 2;
    biHeight := 2;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := 0;
    biSizeImage := 0;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrUsed := 0;
    biClrImportant := 0;
  end;
  fs := TFileStream.Create('test.bmp', fmCreate);


  fs.Write(dk, SizeOf(dk));
  fs.Write(ib, SizeOf(ib));
  fs.Write(Textur32, SizeOf(Textur32));

  fs.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  bit: TBitmap;

  dk: TDateiKopf;
  ib: TInfoBlock;
  fs: TFileStream;
  sl: TStringList;
begin
  bit := TBitmap.Create;
  bit.SetSize(2, 2);
  bit.Canvas.Pixels[0, 0] := $FF;
  bit.Canvas.Pixels[1, 1] := $FF;
  bit.SaveToFile('test.bmp');
  bit.Free;


  fs := TFileStream.Create('test.bmp', fmOpenRead);
  sl := TStringList.Create;

  fs.Read(dk, SizeOf(dk));
  fs.Read(ib, SizeOf(ib));
  //  fs.Write(Textur32, SizeOf(Textur32));

  with dk do begin
    sl.add(IntToStr(bfType));
    sl.add(IntToStr(bfSize));
    sl.add(IntToStr(bfReserved));
    sl.add(IntToStr(bfOffBits));
  end;
  sl.Add('');

  with ib do begin
    sl.add(IntToStr(biSize));
    sl.add(IntToStr(biWidth));
    sl.add(IntToStr(biHeight));
    sl.add(IntToStr(biPlanes));
    sl.add(IntToStr(biBitCount));
    sl.add(IntToStr(biCompression));
    sl.add(IntToStr(biSizeImage));
    sl.add(IntToStr(biXPelsPerMeter));
    sl.add(IntToStr(biYPelsPerMeter));
    sl.add(IntToStr(biClrUsed));
    sl.add(IntToStr(biClrImportant));
  end;

  ShowMessage(sl.Text);

  sl.Free;
  fs.Free;

end;

end.
