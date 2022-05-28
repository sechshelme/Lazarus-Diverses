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
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Elemente: array[0..20] of record
      Edit: TEdit;
      Label_: TLabel;
    end;
    procedure EditEnter(Sender: TObject);
    procedure EditEditingDone(Sender: TObject);
    procedure EditChange(Sender: TObject);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
const
  spalten = 3;
var
  i: integer;
begin
  for i := 0 to Length(Elemente) - 1 do begin
    with Elemente[i] do begin
      Edit := TEdit.Create(Self);
      Label_ := TLabel.Create(Self);
      with Edit do begin
        Tag := i;
        Parent := Panel1;
        Left := i mod spalten * 200 + 110;
        Top := i div spalten * 30 + 5;
        OnEnter := @EditEnter;
        OnEditingDone := @EditEditingDone;
        OnChange := @EditChange;
      end;
      with Label_ do begin
        Tag := i;
        Parent := Panel1;
        Left := i mod spalten * 200 + 5;
        Top := Edit.Top + 5;
        Caption := 'abc';
      end;
    end;
  end;

end;

procedure TForm1.EditEnter(Sender: TObject);
begin
  Elemente[TEdit(Sender).Tag].Edit.Color := clYellow;
end;

procedure TForm1.EditEditingDone(Sender: TObject);
begin
  Elemente[TEdit(Sender).Tag].Edit.Color := clWhite;
end;

procedure TForm1.EditChange(Sender: TObject);
var
  ed: TEdit;
  f: single;
begin
  ed := Elemente[TEdit(Sender).Tag].Edit;
  if (ed.Caption <> '') and TryStrToFloat(ed.Caption, f) then begin
    ed.Color := clYellow;
  end else begin
    ed.Color := $D0D0FF;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  f, f2: single;
  i: integer;
begin
  f := 0.0;
  for i := 0 to Length(Elemente) - 1 do begin
    if TryStrToFloat(Elemente[i].Edit.Caption, f2) then begin
      f := f + f2;
    end;
  end;
  ShowMessage('Die Summe ist: ' + FloatToStr(f));
end;

end.
