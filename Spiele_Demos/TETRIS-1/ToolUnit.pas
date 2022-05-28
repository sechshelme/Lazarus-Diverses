unit ToolUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    Erzeugen1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Erzeugen1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    Bitmap : TBitmap;
  end;

  TKlotzColor = Array[0..20] of Integer;

var
  Form1: TForm1;
  KlotzColor : TKlotzColor = ($FF, $FF00, $FF0000, $FFFF, $FFFF00, $FF00FF, $FFFFFF,
                              $88, $8800, $880000, $8888, $888800, $880088, $888888,
                              $FF, $FF88, $FF8888, $FFFF, $FFFF88, $FF88FF, $BBBBBB);
implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Bitmap := TBitmap.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Bitmap.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.StretchDraw(Rect(0, 0, ClientWidth, ClientHeight), Bitmap);
end;

procedure TForm1.Erzeugen1Click(Sender: TObject);
var
  x, y, i : Integer;
  s : Array[0..3] of TPoint;

  procedure R(x1, y1, x2, y2 : Integer);
  begin
    with Bitmap.Canvas do begin
      MoveTo(x1, y1);
      LineTO(x1, y2);
      LineTO(x2, y2);
      LineTO(x2, y1);
      LineTO(x1, y1);
    end;
  end;

  function col(c, hell : Integer) : Integer;
  var
    f : array[0..2] of Byte;
    i : Integer;
  begin
    f[0] := c and $FF;
    f[1] := (c and $FF00) shr 8;
    f[2] := (c and $FF0000) shr 16;
    if hell = 1 then begin
      for i := 0 to 2 do begin
        if f[i] > $70 then Dec(f[i], $60) else f[i] := 0;
      end;
    end else begin
      for i := 0 to 2 do begin
        if f[i] < $10 then Inc(f[i], $60) else f[i] := $FF;
      end;
    end;
    col := f[0] + f[1] shl 8 + f[2] shl 16;
  end;

begin
  with Bitmap do begin
    Height := 256;
    Width := 672;
    with Canvas do begin
      Brush.Color := $0;
      FillRect(Rect(0, 0, 672, 256));
      for x := 0 to 20 do begin
        y := 0;
        Pen.Color := KlotzColor[x];
        Brush.Color := KlotzColor[x];
        FillRect(Rect(x * 32 + 2, y + 2, x * 32 + 30, y + 30));
        y := 32;
        FillRect(Rect(x * 32, y, x * 32 + 32, y + 32));
        y := 64;
        R(x * 32 + 2, y + 2, x * 32 + 30, y + 30);
        R(x * 32 + 3, y + 3, x * 32 + 29, y + 29);
        R(x * 32 + 6, y + 6, x * 32 + 26, y + 26);
        R(x * 32 + 7, y + 7, x * 32 + 25, y + 25);
        FillRect(Rect(x * 32 + 10, y + 10, x * 32 + 23, y + 23));
        y := 96;
        for i := 0 to 8 do begin
          R(x * 32 + i * 2, y + i * 2, x * 32 + 32 - i * 2, y + 32 - i * 2);
        end;
        y := 128;
        Brush.Color := KlotzColor[x];
        FillRect(Rect(x * 32, y, x * 32 + 32, y + 32));
        Pen.Color := col(KlotzColor[x], 1);
        Brush.Color := col(KlotzColor[x], 1);
        s[0].x := x * 32; s[0].y := y;
        s[1].x := x * 32 + 32; s[1].y := y;
        s[2].x := x * 32; s[2].y := y + 32;
        s[3].x := x * 32 + 32; s[3].y := y + 32;
        Canvas.Polygon([s[1], s[2], s[3], s[0]]);
        y := 160;
        Brush.Color := col(KlotzColor[x], 1);
        FillRect(Rect(x * 32, y, x * 32 + 32, y + 32));
        Brush.Color := KlotzColor[x];
        FillRect(Rect(x * 32 + 3, y + 3, x * 32 + 29, y + 29));
        Pen.Color := col(KlotzColor[x], 0);
        MoveTo(x * 32, y);
        LineTo(x * 32 + 32, y);
        MoveTo(x * 32 + 1, y + 1);
        LineTo(x * 32 + 31, y + 1);
        MoveTo(x * 32 + 2, y + 2);
        LineTo(x * 32 + 30, y + 2);
        MoveTo(x * 32, y + 31);
        LineTo(x * 32 + 32, y + 31);
        MoveTo(x * 32 + 1, y + 30);
        LineTo(x * 32 + 31, y + 30);
        MoveTo(x * 32 + 2, y + 29);
        LineTo(x * 32 + 30, y + 29);
        y := 192;
        Brush.Color := KlotzColor[x];
        FillRect(Rect(x * 32, y, x * 32 + 32, y + 32));
        Brush.Color := col(KlotzColor[x], 1);
        for i := 0 to 3 do begin
          FillRect(Rect(x * 32 + i * 8, y +  0, x * 32 + i * 8 + 4, y + 4));
          FillRect(Rect(x * 32 + i * 8, y +  8, x * 32 + i * 8 + 4, y + 12));
          FillRect(Rect(x * 32 + i * 8, y + 16, x * 32 + i * 8 + 4, y + 20));
          FillRect(Rect(x * 32 + i * 8, y + 24, x * 32 + i * 8 + 4, y + 28));
          FillRect(Rect(x * 32 + i * 8 + 4, y +  4, x * 32 + i * 8 + 8, y + 8));
          FillRect(Rect(x * 32 + i * 8 + 4, y + 12, x * 32 + i * 8 + 8, y + 16));
          FillRect(Rect(x * 32 + i * 8 + 4, y + 20, x * 32 + i * 8 + 8, y + 24));
          FillRect(Rect(x * 32 + i * 8 + 4, y + 28, x * 32 + i * 8 + 8, y + 32));
        end;
        y := 224;
        Pen.Color := KlotzColor[x];
        Brush.Color := KlotzColor[x];
        s[0].x := x * 40; s[0].y := y;
        s[1].x := x * 44 + 64; s[1].y := y;
        s[2].x := x * 28; s[2].y := y + 32;
        s[3].x := x * 32 + 43; s[3].y := y + 32;
        Canvas.Polygon([s[1], s[2], s[3], s[0]]);
      end;
    end;
  end;
  FormPaint(Sender);
  Bitmap.SaveToFile('HG.bmp');
end;

end.
