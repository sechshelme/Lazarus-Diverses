unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{ TForm1 }

function StrToReal(s: string): real; inline;
begin
  Val(s, Result);
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  m2, m3, Kg, l, b, h: real;
  s: string;
begin
  l := StrToReal(Edit1.Text);
  b := StrToReal(Edit2.Text);
  h := StrToReal(Edit3.Text);
  m2 := (l * b + l * h + b * h) * 2 / 10000;
  m3 := (l * b * h) / 1000000;
  Str(m2: 1: 2, s);
  Label13.Caption := s;
  Kg := m2 * StrToReal(Edit4.Text);
  Str(Kg: 1: 2, s);
  Label14.Caption := s;
  Str(m3: 1: 2, s);
  Label16.Caption := s;
end;

initialization
  {$I unit1.lrs}

end.

