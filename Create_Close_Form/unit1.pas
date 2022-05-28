unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    procedure ButtonClick(Sender: TObject);
    procedure fClose(Sender: TObject; var CloseAction: TCloseAction);
  public
    SubForm: TForm;
    Button: TButton;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if not Assigned(SubForm) then begin
    SubForm := TForm.Create(Self);
    SubForm.Caption := 'SubForm';
    SubForm.Left := 200;
    SubForm.Top := 100;
    SubForm.OnClose := @fClose;
    Button := TButton.Create(SubForm);
    Button.Parent := SubForm;
    Button.OnClick := @ButtonClick;
  end else begin
    ShowMessage('no Create');
  end;
  SubForm.Show;
end;

procedure TForm1.ButtonClick(Sender: TObject);
begin
  SubForm.Close;
end;

procedure TForm1.fClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caNone;
  FreeAndNil(SubForm);
end;

end.
