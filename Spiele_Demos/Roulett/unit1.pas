unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, Tisch;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuRadiusClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
  public
    Anzahl, posalt: integer;
    Tisch: TTisch;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  DoubleBuffered := True;

  Tisch := TTisch.Create(Self);
  Tisch.Parent := Self;
  Tisch.Align := alClient;
  Tisch.Anzahl := 15;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  posalt := Random(Anzahl);
  Timer1.Interval := 10;
  Timer1.Enabled := True;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  Tisch.Anzahl := Tisch.Anzahl + 1;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  Tisch.Anzahl := Tisch.Anzahl - 1;
end;

procedure TForm1.MenuRadiusClick(Sender: TObject);
begin
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Inc(posalt);
  if posalt >= Tisch.Anzahl then begin
    posalt := 0;
  end;
  Tisch.SetPos(posalt);
  Tisch.Repaint;
  Timer1.Interval := Timer1.Interval + 2;
  if Timer1.Interval >= 200 then begin
    Timer1.Enabled := False;
  end;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  Tisch.Winkel:=Tisch.Winkel-0.001;
  Tisch.Repaint;
end;

end.
