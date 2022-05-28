unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    Button: array of TButton;
  const
  anzButton = 128 - 32;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
const
  w = 32;
  h = 32;
begin
  SetLength(Button, anzButton);

  for i := 0 to anzButton - 1 do begin
    Button[i] := TButton.Create(Self);
    with Button[i] do begin
      Parent := Self;
      Width := w;
      Height := h;
      Left := (i mod 16) * (w + 2);
      Top := (i div 16) * (h + 2);
      Caption := char(i + 32);
      OnClick := @Button1Click;
    end;
  end;

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  size: integer;
  i: integer;
begin
  size := 0;

  if (Sender is TButton) then begin
    Label1.Caption := TButton(Sender).Caption;
  end;

  //for i := 0 to anzButton - 1 do begin
  //  if Sender = Button[i] then begin
  //    Label1.Caption := Button[i].Caption;
  //    Break;
  //  end;
  //end;

  if Label1.Caption = '+' then begin
    size := 1;
  end;
  if Label1.Caption = '-' then begin
    size := -1;
  end;
  if size <> 0 then begin
    for i := 0 to anzButton - 1 do begin
      Button[i].Width := Button[i].Width + size;
      Button[i].Height := Button[i].Height + size;
      Button[i].Left := (i mod 16) * (Button[i].Width + 2);
      Button[i].Top := (i div 16) * (Button[i].Height + 2);
    end;
  end;

end;

end.
