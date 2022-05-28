unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LazUTF8, BGRABitmap, BGRABitmapTypes, BGRAGradientScanner, BGRATextFX;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    FontDialog1: TFontDialog;
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    BGRABitmap: TBGRABitmap;
    renderer: TBGRATextEffectFontRenderer;

    TexturPos: array[0..256] of record
      Width, Left: integer;
    end;
    procedure CreateFont;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure AssignFontToBGRA(Source: TFont; Dest: TBGRABitmap);
begin
  Dest.FontAntialias := True;

  Dest.FontName := Source.Name;
  Dest.FontStyle := Source.Style;
  Dest.FontOrientation := Source.Orientation;

  case Source.Quality of
    fqNonAntialiased: begin
      Dest.FontQuality := fqSystem;
    end;
    fqAntialiased: begin
      Dest.FontQuality := fqFineAntialiasing;
    end;
    fqProof: begin
      Dest.FontQuality := fqFineClearTypeRGB;
    end;
    fqDefault, fqDraft, fqCleartype, fqCleartypeNatural: begin
      Dest.FontQuality :=
        fqSystemClearType;
    end;
  end;

  Dest.FontHeight := -Source.Height;

  if (Dest.FontQuality = fqFineAntialiasing) or (Dest.FontQuality =
    fqFineClearTypeRGB) and (Dest.FontHeight = 0) then begin
    Dest.FontHeight := round(Source.GetTextHeight('QWERTY') * 1.15);
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  CreateFont;
end;

procedure TForm1.CreateFont;
var
  BGRAGradientScanner: TBGRAGradientScanner;
  w, i: integer;
  c: UnicodeString;

begin
  AssignFontToBGRA(Edit1.Font, BGRABitmap);

  BGRABitmap.FontRenderer := renderer;
  renderer.ShadowVisible := True;
  renderer.OutlineVisible := True;
  renderer.OutlineColor := BGRABlack;
  renderer.OuterOutlineOnly := True;

  TexturPos[0].Left := 0;

  for i := 0 to 255 do begin
    c := UnicodeToUTF8(i);
    w := BGRABitmap.TextSize(c).cx;
    TexturPos[i].Width := w;
    TexturPos[i + 1].Left := w + TexturPos[i].Left;
  end;

  BGRABitmap.SetSize(TexturPos[256].Left, BGRABitmap.FontFullHeight);

  BGRABitmap.Fill(BGRA($00, $FF, $00, $40));

  BGRAGradientScanner := TBGRAGradientScanner.Create(BGRA(255, 255, 0), BGRA(255, 0, 0), gtLinear, PointF(0, 0), PointF(0, BGRABitmap.FontFullHeight), True, True);
  for i := 0 to 255 do begin
    BGRABitmap.TextOut(TexturPos[i].Left, 0, UnicodeToUTF8(i), BGRAGradientScanner);
  end;

  BGRAGradientScanner.Free;
  //  BGRABitmap.SaveToFile('test.bmp');

end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  Repaint;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BGRABitmap := TBGRABitmap.Create;
  renderer := TBGRATextEffectFontRenderer.Create;
  DoubleBuffered := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if FontDialog1.Execute then begin
    Edit1.Font.Assign(FontDialog1.Font);
    CreateFont;
    Repaint;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
//  renderer.Free;
  BGRABitmap.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  UniText: UnicodeString;
  CharIndex, i, cur: integer;
begin
  UniText := UTF8Decode(Edit1.Text);
  cur := 0;
  for i := 1 to Length(UniText) do begin
    CharIndex := byte(UniText[i]);

    Canvas.CopyRect(
      Rect(cur, 0, cur + TexturPos[CharIndex].Width, BGRABitmap.FontFullHeight), BGRABitmap.Canvas,
      Rect(TexturPos[CharIndex].Left, 0, TexturPos[CharIndex].Left + TexturPos[CharIndex].Width, BGRABitmap.FontFullHeight));

    cur := cur + TexturPos[CharIndex].Width;
  end;
end;

end.
