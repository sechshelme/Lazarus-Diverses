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
    Button2: TButton;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    aktFrage: integer;
    Fragen: array of record
      Frage: string;
      Antworten: array[0..2] of string;
      Richtig: integer;
      Gefragt: boolean;
    end;
    Zaehler: integer;
    procedure NextFrage;
  end;

var
  Form1: TForm1;

implementation

const
  maxFragen = 3;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  sl: TStringList;
  sa: TStringArray;
  i, j: integer;
begin
  Randomize;
  sl := TStringList.Create;
  sl.LoadFromFile('fragen.csv');
  if sl.Count < 1 then begin
    ShowMessage('Datei: fragen.csv ungültig');
    Halt;
  end;

  if sl.Count < maxFragen then begin
    ShowMessage('Zu wenig Fragen');
    Halt;
  end;

  SetLength(Fragen, sl.Count);
  for i := 0 to sl.Count - 1 do begin
    sa := sl[i].Split(',');
    if Length(sa) <> 5 then begin
      ShowMessage('Datei: fragen.csv ungültig');
      Halt;
    end;

    with Fragen[i] do begin
      Frage := sa[0];
      for j := 0 to 2 do begin
        Antworten[j] := sa[j + 1];
      end;
      Richtig := StrToInt(sa[4]);
      Gefragt := False;
    end;
  end;

  sl.Free;

  Zaehler := 0;
end;

procedure TForm1.NextFrage;
var
  i: integer;
begin
  repeat
    aktFrage := Random(Length(Fragen));
  until Fragen[aktFrage].Gefragt = False;

  with Fragen[aktFrage] do begin
    gefragt := True;
    RadioGroup1.Caption := Frage;
    RadioGroup1.Items.Clear;
    for i := 0 to 2 do begin
      RadioGroup1.Items.Add(Antworten[i]);
    end;
  end;

  Inc(Zaehler);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  if Zaehler > 0 then begin
    if RadioGroup1.ItemIndex = -1 then begin
      Exit;
    end;
    Button1.Caption := 'Nächste';
    if RadioGroup1.ItemIndex = Fragen[aktFrage].Richtig then begin
      Caption := 'richtig';
    end else begin
      Caption := 'falsch';
    end;
  end;

  if Zaehler >= maxFragen then begin
    if MessageDlg('Frage', 'Nochmals spielen ?', mtInformation, mbYesNo, 0) = mrYes then begin
      Button1.Caption := 'start';
      RadioGroup1.Items.Clear;
      RadioGroup1.Caption := '';

      for i := 0 to Length(Fragen) - 1 do begin
        Fragen[i].Gefragt := False;
      end;

      Zaehler := 0;
    end else begin
      Close;
    end;
  end else begin
    NextFrage;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Close;
end;

end.
