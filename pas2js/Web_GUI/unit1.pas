unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls, StdCtrls;

type

  { TWForm1 }

  TWForm1 = class(TWForm)
    WButton1: TWButton;
    WButton2: TWButton;
    WButton3: TWButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure WButton1Click(Sender: TObject);
    procedure WButton2Click(Sender: TObject);
    procedure WButton3Click(Sender: TObject);
  private
    RadioButton:TWRadioButton;
    panel:TWPanel;
  public

  end;

var
  WForm1: TWForm1;

implementation

{$R *.lfm}

{ TWForm1 }

procedure TWForm1.FormCreate(Sender: TObject);
begin
  Canvas.TextOut(100, 100, 'Hello World');
  Caption:='Meine Webseite';
  RadioButton:=TWRadioButton.Create(Self);
  RadioButton.Parent:=Self;
  RadioButton.Caption:='RadioButton';
  RadioButton.Width:=100;
  RadioButton.Height:=100;

  panel:=TWPanel.Create(Self);
  panel.Parent:=Self;
  panel.Left:=200;
  panel.Top:=200;
end;

procedure TWForm1.FormResize(Sender: TObject);
begin
  Canvas.TextOut(ClientWidth - 200, 100, 'Hello World');
end;

procedure TWForm1.WButton1Click(Sender: TObject);
begin
  Color:=clRed;
end;

procedure TWForm1.WButton2Click(Sender: TObject);
begin
  Color:=clGreen;
end;

procedure TWForm1.WButton3Click(Sender: TObject);
begin
  Color:=clBlue;
end;

end.

