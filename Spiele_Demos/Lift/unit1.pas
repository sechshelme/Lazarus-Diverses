unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

const
  anzEtagen = 9;
  HoeheEtage = 100;
  step = 1;

type

  { TForm1 }

  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelBesetzt: TPanel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    aktPos, Ziel: integer;
    btn: array of record
      Etage, Kabine: TButton;
    end;
    procedure btnEtageClick(Sender: TObject);
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
  DoubleBuffered := True;
  SetLength(btn, anzEtagen);
  Panel1.Height:=90;
  Height:=(anzEtagen-1)*HoeheEtage+Panel1.Height+20;
  for i := 0 to Length(btn) - 1 do begin
    btn[i].Etage := TButton.Create(Self);
    with btn[i].Etage do begin
      Tag := i;
      Parent := Self;
      Top := (Length(btn) - i) * 100 -50;
      Left := 20;
      Width := 100;
      Caption := 'Etage ' + IntToStr(i) + ' Rufen';
      OnClick := @btnEtageClick;
    end;
    btn[i].Kabine := TButton.Create(Panel2);
    with btn[i].Kabine do begin
      Tag := i;
      Parent := Panel2;
      Width := 15;
      Height := 15;
      Left:=i mod 3 *18+2;
      Top:=i div 3 * 18+2;
      Caption := IntToStr(i);
      OnClick := @btnEtageClick;
    end;
  end;
  PanelBesetzt.Font.Color:=clYellow;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Panel1.Top := Height - Panel2.Height - aktPos - 10;
  if aktPos <> Ziel then begin
    PanelBesetzt.Color := clRed;
    PanelBesetzt.Caption:='Besetzt';
  end else begin
    PanelBesetzt.Color := clGreen;
    PanelBesetzt.Caption:='Frei';
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i: integer;
begin
  if aktPos = Ziel then begin
    Timer1.Enabled := False;
    for i := 0 to Length(btn) - 1 do begin
      with btn[i].Etage do begin
        Enabled := True;
      end;
      with btn[i].Kabine do begin
        Enabled := True;
      end;
    end;
  end else begin
    if aktPos > ziel then begin
      Dec(aktPos, step);
    end else begin
      Inc(aktPos, step);
    end;
    Repaint;
  end;
end;

procedure TForm1.btnEtageClick(Sender: TObject);
var
  i: integer;
begin
  Ziel := TButton(Sender).Tag * HoeheEtage;
  if aktPos <> Ziel then begin
    Timer1.Enabled := True;
    for i := 0 to Length(btn) - 1 do begin
      with btn[i].Etage do begin
        Enabled := False;
      end;
      with btn[i].Kabine do begin
        Enabled := False;
      end;
    end;
  end;
end;

end.
