unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, FileUtil,
  Classes,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  ExtCtrls,
  Controls, LazUTF8, Graphics, RichMemo, MyLogForms, Editor,
  BaseUnix, Unix;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtnChapter: TBitBtn;
    BitBtnClipboard: TBitBtn;
    BitBtnFlac: TBitBtn;
    BitBtnJede2ZeileDel: TBitBtn;
    BitBtnSkript: TBitBtn;
    BitBtnSpalte1Del: TBitBtn;
    BitBtnSpalteBackDel: TBitBtn;
    BitBtnTabDel: TBitBtn;
    BitBtnTrackDel: TBitBtn;
    BitBtnZeile1Del: TBitBtn;
    Button1: TButton;
    DTS: TButton;
    EditAlbum: TEdit;
    EditArtist: TEdit;
    EditCapterName: TEdit;
    EditFlacName: TEdit;
    EditGenre: TComboBox;
    EditJahr: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo2: TMemo;

    OpenDialogDTS: TOpenDialog;
    OpenDialogChapter: TOpenDialog;
    OpenDialogFlac: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SourceListBox1: TListBox;
    Splitter1: TSplitter;
    procedure BitBtnChapterClick(Sender: TObject);
    procedure BitBtnSkriptClick(Sender: TObject);
    procedure BitBtnFlacClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure EditorClick(Sender: TObject);
    procedure DTSClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel3Resize(Sender: TObject);
  private
    { Private-Deklarationen }
    ChapterName, FlacName, Pfad: string;
    ifDTS: boolean;
    Ausgabe: TStringList;
    Editor: TEditor;

    function CharTest(s: string; Track: integer): string;
    function FlacOut(Track: integer): string;
    procedure RichtTextColored(sl: TStringList; rm: TRichMemo);
    procedure DTSSkript;
    function Capter: boolean;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses
  Fehlerausgabe, SkriptVorschau;

{$R *.lfm}

const
  eac3to = 'wine /n4800/Download/PC1/eac3to/eac3to.exe';

procedure TForm1.FormCreate(Sender: TObject);
begin
  Editor := TEditor.Create(Memo2);
  FlacName := '';
  ChapterName := '';
  Pfad := '';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Editor.Free;
end;

procedure TForm1.Panel3Resize(Sender: TObject);
var
  w: integer;
begin
  Panel2.Left := Panel3.Width - Panel2.Width - 10;
  w := Panel3.Width - Panel2.Width - 40;
  EditFlacName.Width := w;
  EditCapterName.Width := w;

  w := w - 45;
  EditJahr.Width := w;
  EditAlbum.Width := w;
  EditArtist.Width := w;
  EditGenre.Width := w;
end;

function TForm1.CharTest(s: string; Track: integer): string;
var
  i: integer;
begin
  for i := 1 to Length(s) do begin
    case s[i] of
      '<': begin
        s[i] := '(';
      end;
      '>': begin
        s[i] := ')';
      end;
      '?': begin
        s[i] := '.';
      end;
      '"': begin
        s[i] := #39;
      end;
      ':': begin             //                                        “Prinz Eugen, der edle Ritter”
        s[i] := '.';
      end;
      '|': begin
        s[i] := '-';
      end;
      '/': begin
        s[i] := '-';
      end;
      '\': begin
        s[i] := '-';
      end;
      '*': begin
        s[i] := #36;
      end;
      ' ', 'a'..'z', 'A'..'Z', '0'..'9', '.', '!', ',', '(', ')', '&', '-', '_': begin
      end;
      else begin  // Unbekannntes Zeichen.
        Form2.RichMemo1.Lines.Add('Track:' + IntToStr(Track) + ' Pos:' + IntToStr(i) + ' Code:' + IntToStr(byte(s[i])) + ' Zeichen:-' + s[i] + '- Text:' + s);
      end;
    end;
  end;
  CharTest := s;
end;


function TForm1.FlacOut(Track: integer): string;
var
  s: string;
begin
  if Memo2.Lines.Count >= Track then begin
    s := CharTest(Memo2.Lines[Track - 1], Track);
  end else begin
    s := 'Track';
  end;
  Result := UTF8toConsole(' -o ' + '"' + Format('%.2d', [Track]) + ' - ' + s + '.flac" ' +
    ' -T "TITLE=' + Memo2.Lines[Track - 1] + '"' +
    ' -T "DATE=' + EditJahr.Text + '"' +
    ' -T "ALBUM=' + EditAlbum.Text + '"' +
    ' -T "ARTIST=' + EditArtist.Text + '"' +
    ' -T "GENRE=' + EditGenre.Text + '"' +
    ' -T "TRACKNUMBER=' + Format('%.2d', [Track]) + '"');
end;

procedure TForm1.RichtTextColored(sl: TStringList; rm: TRichMemo);
var
  i: integer;
  col: TColor;
begin
  rm.Clear;

  for i := 0 to sl.Count - 1 do begin
    col := clLtGray;
    if Pos('mv', sl[i]) = 1 then begin
      col := TColor($FF44FF);
    end;
    if Pos(eac3to, sl[i]) = 1 then begin
      col := TColor($FF44BB);
    end;
    if Pos('flac', sl[i]) = 1 then begin
      col := TColor($FF4488);
    end;
    rm.Lines.Add(ConsoleToUTF8(sl[i]));
//    rm.AddColorStr(ConsoleToUTF8(sl[i]), col);
  end;
end;

// Einzel-Dateien

procedure TForm1.DTSSkript;
var
  i: integer;
  s: string;
begin
  for i := 0 to SourceListBox1.Count - 1 do begin
    s := UTF8toConsole(ExtractFileName(SourceListBox1.Items[i]));
    Ausgabe.Add('mv "' + s + '" alt/');
    if ExtractFileExt(UpCase(s)) = '.flac' then begin    // wen Flac kein eac3to nötig
      Ausgabe.Add('flac "alt/' + s + '"' + FlacOut(i + 1));
    end else begin
      Ausgabe.Add('cd alt');
      Ausgabe.Add(eac3to + ' "' + s + '" "' + s + '.flac"');
      Ausgabe.Add('cd ..');
      Ausgabe.Add('flac "alt/' + s + '.flac"' + FlacOut(i + 1));
    end;
  end;
end;

// Flac am Stück mit Chapter-Datei

function TForm1.Capter: boolean;

  function Zeit_von_Chapter_Auslesen(s: string): string;
  var
    min: integer;
  begin
    s := Copy(s, 11, 11);
    min := StrToInt(Copy(s, 1, 2)) * 60 + StrToInt(Copy(s, 4, 2));
    Delete(s, 1, 5);
    s := IntToStr(min) + s;
    s[Pos('.', s)] := DecimalSeparator; // Bei DE ',', bei CH '.' .
    Result := s;
  end;

var
  i: integer;
  Track: integer;
  start, ende: string;
begin
  i := 0;
  Track := 0;
  Ausgabe.Add('#!/bin/bash');
  try
    repeat
      start := Zeit_von_Chapter_Auslesen(SourceListBox1.Items[i]);
      if i = SourceListBox1.Count - 2 then begin
        Ausgabe.Add(UTF8toConsole('flac "' + CharTest(FlacName, Track) + '" --skip=' + start + FlacOut(Track + 1)));
      end else begin
        ende := Zeit_von_Chapter_Auslesen(SourceListBox1.Items[i + 2]);
        Ausgabe.Add(UTF8toConsole('flac "' + CharTest(FlacName, Track) + '" --skip=' + start + ' --until=' + ende + FlacOut(Track + 1)));
      end;
      Inc(i, 2);
      Inc(Track);
    until i >= SourceListBox1.Count;
    Result := True;
  except
    Result := False;
  end;
end;

procedure TForm1.BitBtnChapterClick(Sender: TObject);
var
  FN: string;
begin
  if OpenDialogChapter.Execute then begin
    ifDTS := False;
    SourceListBox1.Sorted := False;
    FN := OpenDialogChapter.FileName;
    SourceListBox1.Items.LoadFromFile(FN);
    ChapterName := ExtractFileName(FN);
    EditCapterName.Text := ChapterName;
    Pfad := ExtractFilePath(FN);
  end;
end;

procedure TForm1.BitBtnFlacClick(Sender: TObject);
var
  FN: string;
begin
  if OpenDialogFlac.Execute then begin
    ifDTS := False;
    FN := OpenDialogFlac.FileName;
    FlacName := ExtractFileName(FN);
    EditFlacName.Text := FlacName;
    Pfad := ExtractFilePath(FN);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  s: TStringList;
begin
  s := TStringList.Create;
  s.Text := UTF8toSys(Memo2.Text);
  s.SaveToFile('mp3tag.txt');
  s.Free;
end;

procedure TForm1.BitBtnSkriptClick(Sender: TObject);
var
  FN: string;

begin
  if SourceListBox1.Count = 0 then begin
    ShowMessage('Keine Dateien geladen');
    Exit;
  end;
  Form2.Show;
  Form2.RichMemo1.Clear;
  Ausgabe := TStringList.Create;
  //  Ausgabe.Add('@echo off');
  if ifDTS then begin
    Ausgabe.Add('#!/bin/bash');
    Ausgabe.Add('mkdir alt');
    DTSSkript;
  end else begin
    if not Capter then begin
      ShowMessage('Capter Fehlerhaft');
      Exit;
    end;
  end;
  Ausgabe.Add('echo Konvertierung abgeschlossen');
  Ausgabe.Add('echo Taste = weiter');
  Ausgabe.Add('read');

  RichtTextColored(Ausgabe, SkriptForm.RichMemo1);

  //  SkriptForm.RichMemo1.Text := ConsoleToUTF8(Ausgabe.Text);
  case SkriptForm.ShowModal of
    mrOk: begin
      FN := Pfad + 'convert' + '.sh';
      Ausgabe.SaveToFile(FN);
            fpChmod (FN,&777);
////            fpexecl (FN,[]);
    end;
  end;
  Ausgabe.Free;
end;

procedure TForm1.EditorClick(Sender: TObject);
begin
  with Editor do begin
    if TButton(Sender).Name = 'BitBtnZeile1Del' then begin
      Zeile1Del;
    end else if TButton(Sender).Name = 'BitBtnJede2ZeileDel' then begin
      Jede2ZeileDel;
    end else if TButton(Sender).Name = 'BitBtnTrackDel' then begin
      TrackDel;
    end else if TButton(Sender).Name = 'BitBtnSpalte1Del' then begin
      Spalte1Del;
    end else if TButton(Sender).Name = 'BitBtnSpalteBackDel' then begin
      SpalteBackDel;
    end else if TButton(Sender).Name = 'BitBtnTabDel' then begin
      TabDel;
    end else if TButton(Sender).Name = 'BitBtnClipboard' then begin
      Clipboard;
    end;
  end;
end;

procedure TForm1.DTSClick(Sender: TObject);
begin
  if OpenDialogDTS.Execute then begin
    ifDTS := True;
    SourceListBox1.Sorted := True;
    SourceListBox1.Items := OpenDialogDTS.Files;
    Pfad := ExtractFilePath(SourceListBox1.Items[0]);
  end;
end;

end.
