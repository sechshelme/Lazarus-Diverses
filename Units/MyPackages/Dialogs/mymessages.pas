unit MyMessages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,Buttons;

type

  { TMessages }

  TMessages = class(TObject)
    class procedure Message(Msg, Caption: string);
    class procedure MemoMessage(Msg, Caption: string);
  end;

var
  MyMessage: TMessages;


implementation


type


  { TMyForm }

  TMyForm = class(TForm)
    procedure CloseButtonClick(Sender: TObject);
  end;

{ TMessages }

class procedure TMessages.Message(Msg, Caption: string);
var
  MyForm: TMyForm;
  MyButton: TButton;
  MyLabel: TLabel;
begin
  MyForm := TMyForm.CreateNew(nil);
  MyForm.SetBounds(100, 100, 600, 700);
  MyForm.Caption := Caption;

  MyLabel := TLabel.Create(MyForm);
  MyLabel.Caption := Msg;
  MyLabel.SetBounds(10, 10, 200, 30);
  MyLabel.Parent := MyForm;

  MyButton := TButton.Create(MyForm);
  MyButton.Caption := 'Close';
  MyButton.SetBounds(10, 40, 200, 30);
  MyButton.Parent := MyForm;

  MyButton.OnClick := @MyForm.CloseButtonClick;

  MyForm.ShowModal;

  FreeAndNil(MyForm);
end;

class procedure TMessages.MemoMessage(Msg, Caption: string);
var
  Form: TMyForm;
  BitButton: TBitBtn;
  Memo: TMemo;
begin
  Form := TMyForm.CreateNew(nil);
  Form.SetBounds(100, 100, 600, 900);
  Form.Caption := Caption;

  Memo := TMemo.Create(Form);
  Memo.Caption := Msg;
  Memo.Align := alTop;
  Memo.Anchors := [akTop, akLeft, akBottom];
  Memo.Height := Form.ClientHeight - 45;
  Memo.ScrollBars:=ssAutoBoth;
  Memo.Parent := Form;

  BitButton := TBitBtn.Create(Form);
  BitButton.Caption := 'Close';
  BitButton.Kind:=bkCancel;
  BitButton.Top := Form.ClientHeight - 35;
  BitButton.Left := Form.ClientWidth - 90;
  BitButton.Anchors := [akRight, akBottom];
  BitButton.Parent := Form;

  BitButton.OnClick := @Form.CloseButtonClick;

  Form.ShowModal;

  FreeAndNil(Form);
end;


{ TMyForm }

procedure TMyForm.CloseButtonClick(Sender: TObject);
begin
//  Close;
  if Sender is TButton then begin
    TForm(TButton(Sender).Parent).Close;
  end;
end;


end.
