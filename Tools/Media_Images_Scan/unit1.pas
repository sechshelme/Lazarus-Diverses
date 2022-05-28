unit unit1;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileUtil, LazFileUtils,// RichMemo,
  MyColorString, MyLogForms;

type
  { TForm1 }
  TForm1 = class(TForm)
    ButtonStart: TButton;
    ButtonAbruch: TButton;
    ButtonSelectDirectory: TButton;
    EditVerzeichniss: TEdit;
    EditPixel: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    CheckBoxThumb: TCheckBox;
    CheckVoransicht: TCheckBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonAbruchClick(Sender: TObject);
    procedure ButtonSelectDirectoryClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Zahler: integer;
    BitmapSize: integer;
    Stop: boolean;
    procedure Suchen(Pfad: string);
    procedure Thumbschreiben(Pfad: string);
    procedure Imagesplitt(Pfad: string);
  public
    ColorMemo: TColorMemo;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  Folder = 'folder.jpg';
  Front = 'Front.jpg';

{$IFDEF MSWINDOWS}
  StartPfad = 'V:\Multimedia\Music';

{$ELSE}
//StartPfad = '/n4800/Multimedia/Music';
//StartPfad = '/n4800/Multimedia/Blu-ray Audio/Naxos';
StartPfad = '/n4800/Multimedia/Blu-ray Audio/Crystal Classics';
{$ENDIF}

procedure TForm1.Thumbschreiben(Pfad: string);
var
  bmp, jpeg: TPicture;
  sl: TStringList;
  i: integer;

begin
  if FileExistsUTF8(Pfad + '/' + Front) then begin
    Jpeg := TPicture.Create;
    jpeg.LoadFromFile(Pfad + '/' + Front);
    bmp := TPicture.Create;
    bmp.Bitmap.Width := BitmapSize;
    bmp.Bitmap.Height := BitmapSize;
    bmp.Bitmap.canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), jpeg.Bitmap);

    bmp.SavetoFile(Pfad + '/' + Folder);

    canvas.StretchDraw(Rect(600, 10, 600 + 64, 10 + 64), jpeg.Bitmap);
    bmp.Free;
    ColorMemo.Lines.Add(Pfad + '/' + Folder, clTeal);
    jpeg.Free;
    Inc(Zahler);

    sl := FindAllDirectories(Pfad + '/', False);  // Noch Unter-Ordner (mehrer CDs ) ?
    for i := 0 to sl.Count - 1 do begin
      if (not FileExistsUTF8(sl[i] + '/' + Front)) and (FileExistsUTF8(Pfad + '/' + Folder)) then begin
        CopyFile(Pfad + '/' + Folder, sl[i] + '/' + Folder, False);
        ColorMemo.Lines.Add(sl[i] + '/' + Folder, clOlive);
      end;
    end;
    sl.Free;
  end;
end;

procedure TForm1.Imagesplitt(Pfad: string);

  procedure Zeichne(Count, pos: integer; bmp, jpeg: TBitmap);
  var
    l, t, w, size: integer;
  begin
    size := trunc(sqrt(Count - 1) + 1);
    w := BitmapSize div size + 1;
    l := round((pos mod size) * (BitmapSize / size));
    t := round((pos div size) * (BitmapSize / size));
    bmp.canvas.StretchDraw(Rect(l + 1, t + 1, l + w - 1, t + w - 1), jpeg);
  end;

var
  i: integer;
  bmp, jpeg: TPicture;
  Datei: string;
  sl: TStringList;

begin
  sl := FindAllDirectories(Pfad + '/', False);
  sl.Sort;

  bmp := TPicture.Create;
  bmp.Bitmap.Width := BitmapSize;
  bmp.Bitmap.Height := BitmapSize;
  jpeg := TPicture.Create;
  for i := 0 to sl.Count - 1 do begin
    Datei := sl[i] + '/' + Front;
    if FileExistsUTF8(Datei) then begin
      jpeg.LoadFromFile(Datei);
    end else if FileExistsUTF8(Pfad + '/' + Front) then begin
      jpeg.LoadFromFile(Pfad + '/' + Front);
    end else begin
      jpeg.Clear;
      jpeg.Bitmap.Width := BitmapSize;
      jpeg.Bitmap.Height := BitmapSize;
      jpeg.Bitmap.Canvas.Brush.Color := $66DD66;
      jpeg.Bitmap.Canvas.Rectangle(30, 30, 226, 226);
    end;
    Zeichne(sl.Count, i, bmp.Bitmap, jpeg.Bitmap);
  end;

  if sl.Count > 0 then begin
    ColorMemo.Lines.Add(Pfad + '/' + Folder, clSkyBlue);
    bmp.SaveToFile(Pfad + '/' + Folder);
    canvas.StretchDraw(Rect(700, 10, 700 + 64, 10 + 64), jpeg.Bitmap);
    Inc(Zahler);
  end;
  jpeg.Free;
  bmp.Free;
  sl.Free;
end;

procedure TForm1.Suchen(Pfad: string);
var
  sl: TStringList;
  i: integer;
begin
  Application.ProcessMessages;

  if Stop then begin
    Exit;
  end;
  if CheckBoxThumb.Checked then begin
    Thumbschreiben(Pfad);
  end;
  if CheckVoransicht.Checked then begin
    Imagesplitt(Pfad);
  end;

  sl := FindAllDirectories(Pfad + '/', False);
  sl.Sort;
  for i := 0 to sl.Count - 1 do begin
    Suchen(sl[i]);
  end;
  sl.Free;
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
var
  i: integer;
begin
  ButtonStart.Enabled := False;
  ButtonAbruch.Enabled := True;
  Stop := False;
  try
    i := StrToInt(EditPixel.Text);
    if i < 16 then begin
      i := 16;
      EditPixel.Text := '16';
    end;
  except
    ShowMessage('Ungültige Zahl');
    i := 16;
    EditPixel.Text := '16';
  end;
  BitmapSize := i;
  Zahler := 0;
  //  ColorMemo.Clear;
  Suchen(EditVerzeichniss.Text);
  ColorMemo.Lines.Add(LineEnding + 'Es wurden ' + IntToStr(Zahler) + ' Dateien kopiert.', clYellow);
  ButtonAbruch.Enabled := False;
  ButtonStart.Enabled := True;
end;

procedure TForm1.ButtonAbruchClick(Sender: TObject);
begin
  Stop := True;
end;

procedure TForm1.ButtonSelectDirectoryClick(Sender: TObject);
begin
  SelectDirectoryDialog1.FileName := EditVerzeichniss.Text;
  if SelectDirectoryDialog1.Execute then begin
    EditVerzeichniss.Text := IncludeTrailingPathDelimiter(SelectDirectoryDialog1.FileName);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Stop := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ColorMemo := TColorMemo.Create(Self);
  with ColorMemo do begin
    Parent := Self;
    Align := alBottom;
    Anchors := [akBottom, akLeft, akRight, akTop];
    top := 100;
  end;
  EditVerzeichniss.Text := StartPfad;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ColorMemo.Free;
end;

end.
