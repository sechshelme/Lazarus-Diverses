unit Editor;

{$mode delphi}

interface

uses
  Classes, StdCtrls,
  SysUtils;

type

  { TEditor }

  TEditor = class(TObject)
  private
    Memo: TMemo;
  public
    constructor Create(m: TMemo);
    procedure Zeile1Del;
    procedure CursorZero;
    procedure Jede2ZeileDel;
    procedure TrackDel;
    procedure Spalte1Del;
    procedure SpalteBackDel;
    procedure TabDel;
    procedure Clipboard;
  end;


implementation

{ TEditor }

constructor TEditor.Create(m: TMemo);
begin
  Memo := m;
end;

procedure TEditor.Zeile1Del;
begin
  Memo.Lines.Delete(0);
  CursorZero;
end;

procedure TEditor.CursorZero;
begin
  Memo.SelStart := 0;
  Memo.SelLength := 0;
end;

procedure TEditor.Jede2ZeileDel;
var
  i: integer;
begin
  for i := 0 to Memo.Lines.Count div 2 do begin
    Memo.Lines.Delete(i + 1);
  end;
  CursorZero;
end;

procedure TEditor.TrackDel;
var
  p, i: integer;
  s: string;
begin
  for i := 0 to Memo.Lines.Count do begin
    s := Memo.Lines[i];
    p := Pos('.', s);
    if p > 4 then begin
      Exit;
    end;
    Delete(s, 1, p);
    Memo.Lines[i] := s;
  end;
  CursorZero;
end;

procedure TEditor.Spalte1Del;
var
  i: integer;
  s: string;
begin
  for i := 0 to Memo.Lines.Count do begin
    s := Memo.Lines[i];
    Delete(s, 1, 1);
    Memo.Lines[i] := s;
  end;
  CursorZero;
end;

procedure TEditor.SpalteBackDel;
var
  i: integer;
  s: string;
begin
  for i := 0 to Memo.Lines.Count do begin
    s := Memo.Lines[i];
    Delete(s, Length(s), 1);
    Memo.Lines[i] := s;
  end;
  CursorZero;
end;

procedure TEditor.TabDel;
var
  p, i: integer;
  s: string;
begin
  for i := 0 to Memo.Lines.Count do begin
    s := Memo.Lines[i];
    p := Pos(#9, s);
    Delete(s, p, 255);
    Memo.Lines[i] := s;
  end;
  CursorZero;
end;

procedure TEditor.Clipboard;
begin
  Memo.Clear;
  Memo.PasteFromClipboard;
  CursorZero;
end;

end.
