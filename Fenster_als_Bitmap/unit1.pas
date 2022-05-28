unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics, GraphType,
  Dialogs, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
var
  ABitmap: TBitmap;
  i, y: integer;

  procedure CopyLine(y: integer);
  var
    pq: pUInt32;
    pz: PByte;
    i: integer;
    bit: byte;
    rev: boolean;
  begin
    pq := pUInt32(Image1.Picture.Bitmap.RawImage.GetLineStart(y));
    pz := ABitmap.RawImage.GetLineStart(y);

    rev := ABitmap.RawImage.Description.BitOrder = riboBitsInOrder;

    if rev then begin
      bit := 1;        // Linux
    end else begin
      bit := 1 shl 7;  // Win
    end;

    for i := 0 to ABitmap.Width - 1 do begin
      if (pq^ and $FF000000) = $FF000000 then begin
        pz^ := pz^ or bit;
      end;
      Inc(pq);

      if rev then begin
        bit := RolByte(bit);
//        bit := bit shl 1;
        if bit = 1 then begin
//          bit := 1;
          Inc(pz);
        end;
      end else begin
        bit := RorByte(bit);
//        bit := bit shr 1;
        if bit = 128 then begin
//          bit := 128;
          Inc(pz);
        end;
      end;

    end;
  end;

var
  td: TDateTime;

begin
  Image1.Picture.LoadFromFile('Splash.bmp');

  if Image1.Picture.Bitmap.PixelFormat <> pf32bit then begin
    Exit;
  end;
  BorderStyle := bsNone;
  Self.Handle;

  Width := Image1.Picture.Bitmap.Width;
  Height := Image1.Picture.Bitmap.Height;

  ABitmap := TBitmap.Create;
  ABitmap.Monochrome := True;
  ABitmap.Width := Width;
  ABitmap.Height := Height;

  td := Now;

  for i := 0 to 999 do begin
    with Image1.Picture.Bitmap do begin
      for y := 0 to Height - 1 do begin
        CopyLine(y);
      end;
    end;
  end;

  ShowMessage(FormatDateTime('zzz', Now - td));

  SetShape(ABitmap);
  ABitmap.FreeImage;

  //for i := 0 to 3000 do begin
  //  Sleep(1);
  //  Application.ProcessMessages;
  //  WriteLn(i);
  //end;

end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
const
  i: integer = 0;
begin
  Inc(i);
  if i >= 500 then begin
    i := 0;
  end;
  Left := i;
  Top := i;
end;

end.
