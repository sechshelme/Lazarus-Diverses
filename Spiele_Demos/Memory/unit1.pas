unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure CreateRandom;
    procedure MemoryMatrixBoxChange(Sender: TObject);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

const
  breite = 6;


type
  TKarte = record
    Wert: integer;       // Bezeichnung der Karte
    Box: TToggleBox;
  end;

  TMemoryMatrix = array[0..breite * breite - 1] of TKarte;

var
  MemoryMatrix: TMemoryMatrix;
  offen: integer;
  zaehler: integer;
  reset: boolean;

procedure TForm1.CreateRandom;
var
  i, p: integer;
begin
  reset := True;
  offen := -1;
  zaehler := 0;

  for i := 0 to Length(MemoryMatrix) - 1 do begin
    MemoryMatrix[i].Wert := -1;
    MemoryMatrix[i].Box.Visible := True;
    MemoryMatrix[i].Box.Checked := False;
    MemoryMatrix[i].Box.Caption := '';
  end;
  for i := 0 to (Length(MemoryMatrix) - 1) div 2 do begin
    repeat
      p := Random(breite * breite);
    until MemoryMatrix[p].Wert = -1;
    MemoryMatrix[p].Wert := i;

    repeat
      p := Random(breite * breite);
    until MemoryMatrix[p].Wert = -1;
    MemoryMatrix[p].Wert := i;
  end;
  reset := False;
end;

procedure TForm1.MemoryMatrixBoxChange(Sender: TObject);
var
  tb: TToggleBox;
begin
  if reset then begin
    Exit;
  end;

  tb := TToggleBox(Sender);

  if offen = tb.Tag then begin
    Exit;
  end;

  if offen = -1 then begin
    offen := tb.Tag;
    tb.Caption := IntToStr(MemoryMatrix[tb.Tag].Wert);
  end else begin
    if MemoryMatrix[tb.Tag].Wert = MemoryMatrix[offen].Wert then begin
      Inc(zaehler, 2);
      MemoryMatrix[tb.Tag].Box.Visible := False;
      MemoryMatrix[offen].Box.Visible := False;
      offen := -1;
    end else begin
      MemoryMatrix[tb.Tag].Box.Caption := IntToStr(MemoryMatrix[tb.Tag].Wert);
      Application.ProcessMessages;
      Sleep(300);
      MemoryMatrix[tb.Tag].Box.Checked := False;
      MemoryMatrix[tb.Tag].Box.Caption := '';
      WriteLn(offen);
      if offen = -1 then begin
        Exit;
      end;
      MemoryMatrix[offen].Box.Checked := False;
      MemoryMatrix[offen].Box.Caption := '';
      offen := -1;
    end;
  end;

  if zaehler >= breite * breite then begin
    CreateRandom;   // neues Spiel
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  size = 40;
var
  i: integer;
begin
  ClientWidth := size * breite;
  ClientHeight := size * breite;
  for i := 0 to Length(MemoryMatrix) - 1 do begin
    MemoryMatrix[i].Box := TToggleBox.Create(Self);
    with MemoryMatrix[i].Box do begin
      Left := (i mod breite) * size + 2;
      Top := (i div breite) * size + 2;
      Width := size - 4;
      Height := size - 4;
      Parent := Self;
      Tag := i;
      OnChange := @MemoryMatrixBoxChange;
    end;
  end;
  CreateRandom;
end;

end.
