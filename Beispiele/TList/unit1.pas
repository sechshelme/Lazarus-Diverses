unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  PVector = ^TVector;
  TVector = record
    case integer of
      1: (ar: array[0..2] of single);
      2: (x, y, z: single);
      3: (r, g, b: single);
  end;

  { TVecList }

  TVecList = class(TList)
  private
    //    function AVecCompare(Item1, Item2: Pointer): integer;
  public
    procedure Add(v: TVector);
    procedure Clear; override;
    procedure Insert(Index: integer; v: TVector);
    procedure Sort;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Add: TButton;
    Ausgabe: TButton;
    Sort: TButton;
    Insert: TButton;
    procedure AddClick(Sender: TObject);
    procedure AusgabeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InsertClick(Sender: TObject);
    procedure SortClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    VecList: TVecList;
  end;

var
  Form1: TForm1;


implementation

{$R *.lfm}

{ TVecList }

procedure TVecList.Add(v: TVector);
var
  pv: PVector;
begin
  new(pv);
  pv^ := v;
  inherited Add(pv);
end;

procedure TVecList.Clear;
var
  i: integer;
  pv: PVector;
begin
  inherited Clear;
  for i := 0 to Count - 1 do begin
    pv := Items[i];
    Dispose(pv);
  end;
  Writeln('VecList.Free');
end;

procedure TVecList.Insert(Index: integer; v: TVector);
var
  pv: PVector;
begin
  new(pv);
  pv^ := v;
  inherited Insert(Index, pv);
end;

function AVecCompare(Item1, Item2: Pointer): integer;
begin
  Result := 0;
  if TVector(Item1^).y > TVector(Item2^).y then begin
    Result := 1;
  end else if TVector(Item1^).y < TVector(Item2^).y then begin
    Result := -1;
  end;
end;

procedure TVecList.Sort;
begin
  inherited Sort(@AVecCompare);  // Hier gibts einen Fehler
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  VecList := TVecList.Create;
end;

procedure TForm1.AddClick(Sender: TObject);
var
  v: TVector;
const
  z: integer = 0;
begin
  Inc(z);
  WriteLn(z);
  v.x := 0;
  v.y := z;
  v.z := 0;
  VecList.Add(v);
end;

procedure TForm1.AusgabeClick(Sender: TObject);
var
  v: PVector;
  i: integer;
begin
  for i := 0 to VecList.Count - 1 do begin
    v := VecList.Items[i];
    WriteLn(v^.y);
  end;
  WriteLn();
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  VecList.Free;
  WriteLn('Programm beendet  <Enter>=weiter');
  ReadLn;
end;

procedure TForm1.InsertClick(Sender: TObject);
var
  v: TVector;
const
  z: integer = 100;
begin
  Inc(z);
  WriteLn(z);
  v.x := 100;
  v.y := z;
  v.z := 100;
  VecList.Insert(0, v);
end;

procedure TForm1.SortClick(Sender: TObject);
begin
  VecList.Sort();
end;


end.
