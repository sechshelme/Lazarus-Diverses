unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, RechnerTasten;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    Buttons: TRechnerTasten;
    Pufferalt, Puffer: string;
    procedure ButtonClick(Sender: TObject; s: string);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Puffer := '';
  Pufferalt := '';
  Buttons := TRechnerTasten.Create(Panel2);
  Buttons.OnButtonClick := @ButtonClick;
  for i := 0 to 9 do begin
    Buttons.Add(IntToStr(i));
  end;
  Buttons.Add('+');
  Buttons.Add('-');
  Buttons.Add('/');
  Buttons.Add('*');
  Buttons.Add('=');
  Buttons.Add('.');
  Buttons.Add('C');
end;

function sTof(s: string): single;
var
  e: integer;
begin
  val(s, Result, e);
  if e <> 0 then begin
    Result := 0.0;
  end;
end;


procedure TForm1.ButtonClick(Sender: TObject; s: string);
begin
  if s = '+' then begin
    Puffer := FloatToStr(sTof(Pufferalt) + sTof(Puffer));
    Pufferalt := Puffer;
    Panel1.Caption := Puffer;
    Puffer := '';
  end else if s = '-' then begin
    Puffer := FloatToStr(sTof(Pufferalt) - sTof(Puffer));
    Pufferalt := Puffer;
    Panel1.Caption := Puffer;
    Puffer := '';
  end else if s = '*' then begin
    Puffer := FloatToStr(sTof(Pufferalt) * sTof(Puffer));
    Pufferalt := Puffer;
    Panel1.Caption := Puffer;
    Puffer := '';
  end else if s = '/' then begin
    Puffer := FloatToStr(sTof(Pufferalt) / sTof(Puffer));
    Pufferalt := Puffer;
    Panel1.Caption := Puffer;
    Puffer := '';
  end else if s = 'C' then begin
    Pufferalt := '';
    Panel1.Caption := '';
    Puffer := '';
  end else if s[1] in ['0'..'9', '.'] then begin
    Puffer := Puffer + s;
    Panel1.Caption := Puffer;
  end;
  Label1.Caption:='Puffer:'+Puffer;
  Label2.Caption:='PAlt:'+Pufferalt;
end;

end.
