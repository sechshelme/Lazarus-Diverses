unit Hauptfor;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ComCtrls, ExtCtrls, HISCORE, Buttons{, GradForm};

type
  THauptFormular = class(TForm)
    MainMenu1: TMainMenu;
    Datei1: TMenuItem;
    neu1: TMenuItem;
    Timer1: TTimer;
    Hilfe1: TMenuItem;
    About1: TMenuItem;
    StatusBar1: TStatusBar;
    Optionen1: TMenuItem;
    Hintergrund1: TMenuItem;
    Next1: TMenuItem;
    Statistik1: TMenuItem;
    Hilfslinien1: TMenuItem;
    Hiscoreanschauen1: TMenuItem;
    Hiscorelschen1: TMenuItem;
    ColorDialog1: TColorDialog;
    Hilfethemen1: TMenuItem;
    SpielOptionen1: TMenuItem;
    function Pruefen: boolean;
    function NeuerKlotz: boolean;
    procedure ZKlotz(x, y, Nr: integer);
    procedure ZeichenKlotz(pos: TPoint);
    procedure PrintStatusZeile;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure neu1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure About1Click(Sender: TObject);
    procedure Next1Click(Sender: TObject);
    procedure Statistik1Click(Sender: TObject);
    procedure Hiscoreanschauen1Click(Sender: TObject);
    procedure Hiscorelschen1Click(Sender: TObject);
    procedure Hintergrund1Click(Sender: TObject);
    procedure SpielOptionen1Click(Sender: TObject);
    procedure Hilfslinien1Click(Sender: TObject);
    procedure Hilfethemen1Click(Sender: TObject);
  private
    KlotzPosneu, KlotzPosalt: TPoint;
  public
  end;

var
  HauptFormular: THauptFormular;

const
  anzKloetze = 21;
  Spielaktiv: boolean = False;
  maxX: integer = 10;
  maxY: integer = 20;
  Seite = 32;

type
  TKlotz = array[0..3, 0..3] of byte;
  TKloetze = array[0..anzKloetze - 1] of TKlotz;
  TFelder = array[0..100, 0..100] of byte;
  TKlotzColor = array[1..anzKloetze] of integer;

  TOptionen = record
    Hintergrund: record
      Typ, col: integer;
    end;
    KlotzGrafik: integer;
    Virus: record
      plus, minus: byte;
    end;
    StartLevel,
    vorbau: integer;
    Klotzart: integer;
    Next: boolean;
    Statistik: boolean;
    Hilfslinien: boolean;
  end;

  TSpielstatus = record
    Level, Punkte, Linien: integer;
  end;

const
  Kloetze: TKloetze = (
    ((0, 0, 1, 0), (0, 0, 1, 0), (0, 0, 1, 0), (0, 0, 1, 0)),
    ((0, 0, 0, 0), (0, 2, 2, 0), (0, 2, 2, 0), (0, 0, 0, 0)),
    ((0, 3, 3, 0), (0, 3, 0, 0), (0, 3, 0, 0), (0, 0, 0, 0)),
    ((0, 4, 4, 0), (0, 0, 4, 0), (0, 0, 4, 0), (0, 0, 0, 0)),
    ((0, 0, 5, 0), (0, 5, 5, 0), (0, 5, 0, 0), (0, 0, 0, 0)),
    ((0, 6, 0, 0), (0, 6, 6, 0), (0, 0, 6, 0), (0, 0, 0, 0)),
    ((0, 0, 0, 0), (0, 7, 0, 0), (7, 7, 7, 0), (0, 0, 0, 0)),
    ((0, 0, 0, 0), (0, 8, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0)),
    ((0, 0, 0, 0), (0, 9, 9, 0), (0, 9, 0, 0), (0, 0, 0, 0)),
    ((0, 0, 0, 0), (0, 10, 10, 0), (0, 0, 0, 0), (0, 0, 0, 0)),
    ((0, 0, 0, 0), (0, 11, 11, 11), (0, 0, 0, 0), (0, 0, 0, 0)),
    ((0, 0, 0, 0), (0, 0, 12, 0), (0, 12, 0, 12), (0, 0, 0, 0)),
    ((0, 0, 0, 0), (0, 13, 0, 0), (0, 0, 13, 0), (0, 0, 0, 0)),
    ((0, 0, 14, 0), (0, 14, 0, 0), (14, 0, 0, 0), (0, 0, 0, 0)),
    ((0, 15, 15, 0), (0, 15, 15, 0), (0, 15, 15, 0), (0, 0, 0, 0)),
    ((0, 0, 0, 0), (16, 0, 16, 0), (16, 16, 16, 0), (0, 0, 0, 0)),
    ((0, 17, 0, 0), (0, 17, 0, 0), (0, 17, 17, 17), (0, 0, 0, 0)),
    ((0, 18, 18, 0), (0, 18, 18, 0), (0, 18, 0, 0), (0, 0, 0, 0)),
    ((0, 0, 19, 0), (0, 0, 19, 0), (0, 0, 19, 0), (0, 19, 19, 0)),
    ((0, 20, 0, 0), (0, 20, 0, 0), (0, 20, 0, 0), (0, 20, 20, 0)),
    ((0, 0, 21, 0), (0, 21, 21, 21), (0, 0, 21, 0), (0, 0, 0, 0)));

  KlotzColor: TKlotzColor = (
    $FF, $FF00, $FF0000, $FFFF, $FFFF00, $FF00FF, $FFFFFF,
    $88, $8800, $880000, $8888, $888800, $880088, $888888,
    $FF, $FF88, $FF8888, $FFFF, $FFFF88, $FF88FF, $444444);

var
  aktKlotz: record
    Typ: byte;
    Neu, Raster: TKlotz;
  end;
  Felder: record
    neu, alt, akt: TFelder;
  end;
  Optionen: TOptionen;
  Spielstatus: TSpielstatus;

implementation

uses
  About, Next, STATISTI, HINTERGR, OPTI;

{$R *.lfm}

procedure THauptFormular.ZKlotz;
var
  x1, y1, x2, y2: integer;
begin
  with Canvas do begin
    x1 := ClientWidth * x div maxX;
    x2 := ClientWidth div maxX + 1;
    y1 := (ClientHeight - StatusBar1.Height) * y div maxY;
    y2 := (ClientHeight - StatusBar1.Height) div maxY + 1;
    StretchBlt(Handle, x1, y1, x2, y2,
      KlotzBitmap[Nr].Canvas.Handle, 0, 0, Seite, Seite, srccopy);
  end;
end;

procedure THauptFormular.ZeichenKlotz;
var
  x1, y1: integer;

begin
  Felder.neu := Felder.alt;
  with Klotzposneu, aktKlotz do begin
    for x1 := x to x + 3 do begin
      for y1 := y to y + 3 do begin
        if Raster[x1 - x, y1 - y] <> 0 then begin
          Felder.neu[x1, y1] := Raster[x1 - x, y1 - y];
        end;
      end;
    end;
  end;
  for x1 := 0 to maxX do begin
    for y1 := 0 to maxY do begin
      if Felder.akt[x1, y1] <> Felder.neu[x1, y1] then begin
        ZKlotz(x1, y1, Felder.neu[x1, y1]);
      end;
    end;
  end;
  Felder.akt := Felder.neu;
  Klotzposalt := Klotzposneu;
  Repaint;
end;

function THauptFormular.Pruefen;
var
  x1, y1: integer;

begin
  Pruefen := False;
  with KlotzPosneu do begin
    for x1 := 0 to 3 do begin
      for y1 := 0 to 3 do begin
        if (aktKlotz.Raster[x1, y1] <> 0) then begin
          if not ((x1 + x in [0..maxX - 1]) and (y1 + y in [0..maxY - 1])) then begin
            Exit;
          end;
          if Felder.alt[x1 + x, y1 + y] <> 0 then begin
            Exit;
          end;
        end;
      end;
    end;
  end;
  Pruefen := True;
end;

function THauptFormular.NeuerKlotz;

  procedure Abbauen(X, Y: byte);
  var
    i: byte;
  begin
    for i := Y downto 1 do begin
      Felder.neu[X, i] := Felder.neu[X, i - 1];
    end;
    Felder.neu[X, 0] := 0;
  end;

  function Reihe: integer;
  var
    X, Y: byte;
    M: boolean;
    R: TRect;

    procedure Invert(y: integer);
    var
      i: integer;
      x1, y1, x2, y2: integer;
    begin
      x1 := 0;
      x2 := ClientWidth;
      y1 := (ClientHeight - StatusBar1.Height) * y div maxY;
      y2 := (ClientHeight - StatusBar1.Height) div maxY;
      for i := 1 to 8 do begin
        BitBlt(Canvas.Handle, x1, y1, x2, y2, Canvas.Handle, x1, y1, dstinvert);
        //        messagebeep(0);
      end;
    end;

  begin
    R := ClientRect;
    Dec(R.Bottom, StatusBar1.Height);
    Result := 0;
    for Y := 0 to maxY do begin
      M := True;
      for X := 0 to maxX - 1 do begin
        if Felder.neu[X, Y] = 0 then begin
          M := False;
        end;
      end;
      if M then begin
        Invert(Y);
        for X := 0 to maxX do begin
          Abbauen(X, Y);
        end;
        Inc(Result);
      end;
    end;
  end;

var
  i: integer;

begin
  with aktKlotz do begin
    KlotzPosneu.X := maxX div 2 - 2;
    KlotzPosneu.Y := 0;
    KlotzPosalt := KlotzPosneu;
    Raster := Neu;
    i := 0;
    repeat
      Typ := Random(anzKloetze);
      case Optionen.Klotzart of
        1: begin
          if Typ in [0..6] then begin
            i := 1;
          end;
        end;
        2: begin
          if Typ in [0..13] then begin
            i := 1;
          end;
        end;
        3: begin
          if Typ in [0..6, 14..20] then begin
            i := 1;
          end;
        end;
        4: begin
          i := 1;
        end;
      end;
    until i = 1;
    Neu := Kloetze[Typ];
    NextBox.Refresh;
    with StatistikBox do begin
      Inc(Anz[Typ]);
      AnzLabel[Typ].Caption := IntToStr(Typ);
    end;
  end;
  with Spielstatus do begin
    for i := 1 to Reihe do begin
      Inc(Punkte, (Level + 1) * 1000 * i);
      Inc(Linien);
    end;
    Level := Linien div 10;
    with Optionen do begin
      if Level < StartLevel then begin
        Level := StartLevel;
      end;
    end;
    i := 1000 - (Level * 50);
    if i < 50 then begin
      i := 50;
    end;
    Timer1.Interval := i;
  end;
  Felder.alt := Felder.neu;
  if not Pruefen then begin
    NeuerKlotz := False;
    Timer1.Enabled := False;
    Spielaktiv := False;
    HiscoreBox.Pruefen;
  end else begin
    NeuerKlotz := True;
  end;
  Inc(Spielstatus.Punkte, 10);
  PrintStatusZeile;
end;

procedure THauptFormular.PrintStatusZeile;
begin
  with StatusBar1, Spielstatus do begin
    Panels[0].Text := 'Level: ' + IntToStr(Level);
    Panels[1].Text := 'Punkte: ' + IntToStr(Punkte);
    Panels[2].Text := 'Linien: ' + IntToStr(Linien);
  end;
end;

procedure THauptFormular.FormCreate(Sender: TObject);
begin
  Randomize;
  KlotzBitmap[0] := TBitmap.Create;
  Left := 50;
  Height := 50;
  ClientHeight := Seite * maxY - 1;
  ClientWidth := Seite * maxX;
  with KlotzBitmap[0], Optionen, Hintergrund do begin
    Next := True;
    Statistik := False;
    Hilfslinien := False;
    with Virus do begin
      plus := 0;
      minus := 0;
    end;
    vorbau := 0;
    StartLevel := 0;
    Klotzart := 1;
    Width := Seite;
    Height := Seite;
    col := $000060;
    Typ := 0;
    with Canvas do begin
      Brush.Color := col;
      FillRect(Rect(0, 0, Seite, Seite));
    end;
  end;
  FillChar(Spielstatus, SizeOf(Spielstatus), 0);
  PrintStatusZeile;
  Spielstatus.Level := 0;
end;

procedure THauptFormular.FormPaint(Sender: TObject);

  function PosX: integer;
  var
    x, y: integer;
  begin
    for x := 3 downto 0 do begin
      for y := 3 downto 0 do begin
        if aktKlotz.Raster[x, y] > 0 then begin
          posX := x;
          Exit;
        end;
      end;
    end;
    PosX := 0;
  end;

var
  x, y: integer;

begin
  if Visible then begin
    with Canvas do begin

      for x := 0 to maxX do begin
        for y := 0 to maxY do begin
          ZKlotz(x, y, Felder.neu[x, y]);
        end;
      end;
      if Optionen.Hilfslinien and Spielaktiv then begin
        with Canvas do begin
          Brush.Color := $FFFFFF;
          Pen.Color := $000000;
          Pen.Style := psDot;
          x := ClientWidth * (KlotzPosneu.x + PosX + 1) div maxX;
          MoveTo(x, ClientHeight);
          LineTo(x, 0);
          Pen.Style := psSolid;
        end;
      end;
    end;
  end;
end;

procedure THauptFormular.FormResize(Sender: TObject);
begin
  FormPaint(Sender);
end;

procedure THauptFormular.neu1Click(Sender: TObject);
var
  x, y: integer;
begin
  FillChar(Felder, SizeOf(Felder), 0);
  FillChar(StatistikBox.Anz, SizeOf(StatistikBox.Anz), 0);
  with aktKlotz do begin
    Typ := Random(7);
    Neu := Kloetze[Typ];
    Inc(StatistikBox.Anz[Typ]);
  end;
  with Spielstatus do begin
    Punkte := 0;
    Linien := 0;
    Level := Optionen.StartLevel;
  end;
  for x := 0 to maxX do begin
    for y := 0 to maxY do begin
      ZKlotz(x, y, Felder.neu[x, y]);
    end;
  end;
  PrintStatusZeile;
  NextBox.Left := Left + Width + 20;
  NextBox.Top := Top;
  NextBox.Color := Optionen.Hintergrund.col;
  if Next1.Checked then begin
    NextBox.Show;
  end;
  NextBox.Refresh;
  StatistikBox.Left := Left + Width + 20;
  StatistikBox.Top := Top + 160;
  StatistikBox.Color := Optionen.Hintergrund.col;
  if Statistik1.Checked then begin
    StatistikBox.Show;
  end;
  with Optionen do begin
    for y := maxY downto maxY - vorBau do begin
      for x := 0 to maxX do begin
        if Random(6) > 1 then begin
          Felder.neu[x, y] := Random(21) + 1;
        end;
      end;
    end;
  end;
  Show;
  NeuerKlotz;
  Spielaktiv := True;
  Timer1.Enabled := True;
end;

procedure THauptFormular.Timer1Timer(Sender: TObject);
var
  i, x, y, z: integer;
begin
  with Optionen.Virus do begin
    if Random(3) + 1 > 3 - plus then begin
      x := Random(maxX);
      i := 3;
      while (Felder.alt[x, i] = 0) and (i <> maxY) do begin
        Inc(i);
      end;
      i := maxY - i + 2;
      z := 100;
      repeat
        Dec(z);
        y := maxY - Random(i);
      until (Felder.alt[x, y] = 0) or (z = 0);
      if z <> 0 then begin
        Felder.alt[x, y] := Random(7) + 1;
      end;
    end;
    if Random(3) + 1 > 3 - minus then begin
      z := 100;
      repeat
        Dec(z);
        x := Random(maxX);
        y := Random(maxY);
      until (Felder.alt[x, y] <> 0) or (z = 0);
      if z <> 0 then begin
        Felder.alt[x, y] := 0;
      end;
    end;
  end;
  with KlotzPosneu do begin
    Inc(y);
    if not Pruefen then begin
      Dec(y);
      NeuerKlotz;
    end;
    ZeichenKlotz(Klotzposalt);
  end;
end;

procedure THauptFormular.FormKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
var
  altk, dummy: TKlotz;
  i, k: integer;
begin
  if Spielaktiv then begin
    with KlotzPosneu do begin
      case Key of
        32: begin
          repeat
            Inc(y);
          until not Pruefen;
          Dec(y);
        end;
        37: begin
          Dec(x);
        end;
        38: begin
          with aktKlotz do begin
            altk := Raster;
            for i := 0 to 3 do begin
              for k := 0 to 3 do begin
                dummy[i, k] := Raster[3 - k, i];
              end;
            end;
            Raster := dummy;
          end;
        end;
        39: begin
          Inc(x);
        end;
        40: begin
          Inc(y);
        end;
      end;
      if not Pruefen then begin
        Klotzposneu := Klotzposalt;
        case Key of
          38: begin
            aktKlotz.Raster := altk;
          end;
          40: begin
            Dec(y);
            NeuerKlotz;
          end;
        end;
      end;
      ZeichenKlotz(Klotzposalt);
    end;
  end;
end;

procedure THauptFormular.About1Click(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure THauptFormular.Next1Click(Sender: TObject);
begin
  with Optionen do begin
    Next := not Next;
    Next1.Checked := Next;
    if Next then begin
      NextBox.Show;
    end else begin
      NextBox.Hide;
    end;
  end;
end;

procedure THauptFormular.Statistik1Click(Sender: TObject);
begin
  with Optionen do begin
    Statistik := not Statistik;
    Statistik1.Checked := Statistik;
    if Statistik then begin
      StatistikBox.Show;
    end else begin
      StatistikBox.Hide;
    end;
  end;
end;

procedure THauptFormular.Hilfslinien1Click(Sender: TObject);
begin
  with Optionen do begin
    Hilfslinien := not Hilfslinien;
    Hilfslinien1.Checked := Hilfslinien;
  end;
end;

procedure THauptFormular.Hiscoreanschauen1Click(Sender: TObject);
begin
  HiscoreBox.ShowModal;
end;

procedure THauptFormular.Hiscorelschen1Click(Sender: TObject);
begin
  if Application.MessageBox('Sind Sie sicher ?',
    'Tetris - Hiscore löschen', mb_yesno or mb_iconquestion) = id_yes then begin
    HiscoreBox.Erzeugen;
  end;
  ;
end;

procedure THauptFormular.Hintergrund1Click(Sender: TObject);
begin
  Darstellungsbox.ShowModal;
end;

procedure THauptFormular.SpielOptionen1Click(Sender: TObject);
begin
  if SpielOptionenBox.ShowModal = mrOk then begin
    neu1Click(Sender);
  end;
end;

procedure THauptFormular.Hilfethemen1Click(Sender: TObject);
begin
//    Application.HelpCommand(help_finder, 0);
end;

end.
