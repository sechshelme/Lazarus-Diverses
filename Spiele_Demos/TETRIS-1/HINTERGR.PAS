unit HINTERGR;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls{, GradForm};

type
  TDarstellungsbox = class(TForm)
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox2: TGroupBox;
    PaintBox1: TPaintBox;
    GroupBox1: TGroupBox;
    BitBtn9: TBitBtn;
    BitBtn12: TBitBtn;
    TabSheet2: TTabSheet;
    GroupBox3: TGroupBox;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label2: TLabel;
    Label8: TLabel;
    TrackBar2: TTrackBar;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    TabSheet3: TTabSheet;
    GroupBox4: TGroupBox;
    TrackBar3: TTrackBar;
    GroupBox5: TGroupBox;
    TrackBar4: TTrackBar;
    GroupBox6: TGroupBox;
    TrackBar5: TTrackBar;
    Panel1: TPanel;
    Image10: TImage;
    TabSheet4: TTabSheet;
    GroupBox7: TGroupBox;
    TrackBar6: TTrackBar;
    Image11: TImage;
    GroupBox8: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    TrackBar7: TTrackBar;
    procedure KlotzArtladen(art : Integer);
    procedure Vor;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
  private
    Bit : TBitmap;
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Darstellungsbox: TDarstellungsbox;
  KlotzBitmap    : Array[0..21] of TBitmap;

implementation

{$R *.lfm}

uses
  Hauptfor, Next, STATISTI;

procedure TDarstellungsbox.KlotzArtladen;
var
  x1, y1, x : Integer;
begin
  for x := 0 to AnzKloetze - 1 do with Image10.Picture.Bitmap.Canvas do begin
    KlotzBitmap[x + 1].Canvas.Draw(0, 0, KlotzBitmap[0]);
    for x1 := 0 to Seite do for y1 := 0 to Seite do
      if Pixels[x * Seite + x1, art * Seite + y1] <> 0 then
        KlotzBitmap[x + 1].Canvas.Pixels[x1, y1] := Pixels[x * Seite + x1, art * Seite + y1];
  end;
end;

procedure TDarstellungsbox.Vor;
var
  x ,y : Integer;
begin
  for x := 0 to 5 do for y := 0 to 5 do
    PaintBox1.Canvas.Draw(x * Seite, y * Seite, Bit);
end;

procedure TDarstellungsbox.FormCreate(Sender: TObject);
var
  x : Integer;
begin
  for x := 1 to AnzKloetze do begin
    KlotzBitmap[x] := TBitmap.Create;
    with KlotzBitmap[x] do begin
      Width := Seite;
      Height := Seite;
    end;
  end;
  Bit := TBitmap.Create;
  Bit.Width := Seite;
  Bit.Height := Seite;
  KlotzArtladen(0);
end;

procedure TDarstellungsbox.FormDestroy(Sender: TObject);
var
  x : Integer;
begin
  for x := 0 to AnzKloetze do begin
    KlotzBitmap[x].Free;
  end;
  Bit.Free;
end;

procedure TDarstellungsbox.BitBtn10Click(Sender: TObject);
begin
  with Optionen do begin
    with Hintergrund do begin
      Typ := TrackBar2.Position;
      col := Panel1.Color;
    end;
    KlotzBitmap[0].Canvas.Draw(0, 0, Bit);
    KlotzGrafik := TrackBar6.Position;
    KlotzArtladen(KlotzGrafik);
  end;
  NextBox.Color := Optionen.Hintergrund.col;
  StatistikBox.Color := Optionen.Hintergrund.col;
  with HauptFormular do begin
    ClientWidth := TrackBar1.Position * maxX;
    ClientHeight := TrackBar7.Position * maxY - 1;
    NextBox.Left := Left + Width + 20;
    NextBox.Top := Top;
    StatistikBox.Left := Left + Width + 20;
    StatistikBox.Top := Top + 160;
  end;
end;

procedure TDarstellungsbox.PaintBox1Paint(Sender: TObject);
begin
  Vor;
end;

procedure TDarstellungsbox.FormShow(Sender: TObject);
var
  x : Integer;
begin
  Bit.Canvas.Draw(0, 0, KlotzBitmap[0]);
  with Optionen.Hintergrund do begin
    TrackBar2.Position := Typ;
    TrackBar3.Position := col and $0000FF;
    TrackBar4.Position := (col and $00FF00) shr 8;
    TrackBar5.Position := (col and $FF0000) shr 16;
    Panel1.Color := col;
  end;
  TrackBar1.Position := HauptFormular.ClientWidth div maxX + 1;
  TrackBar7.Position := HauptFormular.ClientHeight div maxY + 1;
  TrackBar6.Position := Optionen.KlotzGrafik;
  with Image11.Picture.Bitmap do begin
    Width := 409;
    Height := Seite;
    with Canvas do begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0, 0, 409, Seite));
      for x := 0 to 7 do begin
        BitBlt(Handle, x * 54, 0, Seite, Seite, Image10.Picture.Bitmap.Canvas.Handle,
          0, x * Seite, SrcCopy);
      end;
    end;
  end;
end;

procedure TDarstellungsbox.BitBtn12Click(Sender: TObject);
begin
  TrackBar1.Position := Seite;
  TrackBar7.Position := Seite;
  TrackBar2.Position := 0;
  TrackBar3.Position := $60;
  TrackBar4.Position := 0;
  TrackBar5.Position := 0;
  TrackBar6.Position := 0;
  with Optionen do begin
    Panel1.Color := $000060;
    with Bit.Canvas do begin
      Brush.Color := $000060;
      FillRect(Rect(0, 0, Seite, Seite));
    end;
    Vor;
  end;
end;

procedure TDarstellungsbox.TrackBar2Change(Sender: TObject);
begin
  case TrackBar2.Position of
    0 : with Bit.Canvas do begin
          Brush.Color := Panel1.Color;
          FillRect(Rect(0, 0, Seite, Seite));
        end;
    1 : Bit.Canvas.Draw(0, 0, Image2.Picture.Bitmap);
    2 : Bit.Canvas.Draw(0, 0, Image3.Picture.Bitmap);
    3 : Bit.Canvas.Draw(0, 0, Image4.Picture.Bitmap);
    4 : Bit.Canvas.Draw(0, 0, Image5.Picture.Bitmap);
    5 : Bit.Canvas.Draw(0, 0, Image6.Picture.Bitmap);
    6 : Bit.Canvas.Draw(0, 0, Image7.Picture.Bitmap);
    7 : Bit.Canvas.Draw(0, 0, Image8.Picture.Bitmap);
    8 : Bit.Canvas.Draw(0, 0, Image9.Picture.Bitmap);
  end;
  Vor;
end;

procedure TDarstellungsbox.TrackBar3Change(Sender: TObject);
begin
  Panel1.Color := (Panel1.Color and $FFFF00) or TrackBar3.Position;
  with Bit.Canvas do begin
    Brush.Color := Panel1.Color;
    FillRect(Rect(0, 0, Seite, Seite));
  end;
  Vor;
end;

procedure TDarstellungsbox.TrackBar4Change(Sender: TObject);
begin
  Panel1.Color := (Panel1.Color and $FF00FF) or (TrackBar4.Position shl 8);
  with Bit.Canvas do begin
    Brush.Color := Panel1.Color;
    FillRect(Rect(0, 0, Seite, Seite));
  end;
  Vor;
end;

procedure TDarstellungsbox.TrackBar5Change(Sender: TObject);
begin
  Panel1.Color := (Panel1.Color and $00FFFF) or (TrackBar5.Position shl 16);
  with Bit.Canvas do begin
    Brush.Color := Panel1.Color;
    FillRect(Rect(0, 0, Seite, Seite));
  end;
  Vor;
end;

end.
