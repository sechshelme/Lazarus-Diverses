unit MyLogForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Buttons, Forms, StdCtrls, Clipbrd, Dialogs, LazUTF8,// RichMemo,
  MyColorString;

type
  { TRichMemoHelper }

//  TRichMemoHelper = class helper for TRichMemo
//    procedure AddColorStr(s: string; col: TColor);
//  end;

  { TLogForm }

  TLogForm = class(TForm)
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Add(s: string; col: TColor = clMedGray);
  private
    RichMemo: TColorMemo;
    CloseButton, ClipbrdButton, SaveButton: TBitBtn;
    SaveDialog: TSaveDialog;
    procedure FormResize(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure ClipbrdClick(Sender: TObject);
  end;

var
  LogForm: TLogForm;

implementation

{ TRichMemoHelper }

//procedure TRichMemoHelper.AddColorStr(s: string; col: TColor);
//var
//  p: integer;
//begin
//  p := UTF8Length(Text);
//  Lines.Add(s);
//  SetRangeColor(p, UTF8Length(s), col);
//end;

{ TLogForm }

constructor TLogForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Width := 800;
  Height := 600;
  Caption := 'Logs';
  OnResize := @FormResize;

  SaveDialog := TSaveDialog.Create(TheOwner);
  SaveDialog.Filter := 'Text (*.txt)|*.txt';

  RichMemo := TColorMemo.Create(TheOwner);
  RichMemo.Align := alTop;
  RichMemo.Parent := Self;
  RichMemo.Color := clBlack;
//  RichMemo.ScrollBars := ssAutoBoth;
//  RichMemo.WordWrap := False;

  RichMemo.Font.Name := 'Consolas';
  RichMemo.Font.Size := 8;
  RichMemo.Font.Color := clSilver;
  RichMemo.Font.Style := [TFontStyle.fsBold];

  SaveButton := TBitBtn.Create(TheOwner);
  SaveButton.Parent := Self;
  SaveButton.Height := 25;
  SaveButton.Kind := bkCustom;
  SaveButton.Caption := 'Speichern';
  SaveButton.OnClick := @SaveClick;

  ClipbrdButton := TBitBtn.Create(TheOwner);
  ClipbrdButton.Parent := Self;
  ClipbrdButton.Height := 25;
  ClipbrdButton.Kind := bkRetry;
  ClipbrdButton.Caption := 'Clipbrd';
  ClipbrdButton.OnClick := @ClipbrdClick;

  CloseButton := TBitBtn.Create(TheOwner);
  CloseButton.Parent := Self;
  CloseButton.Height := 25;
  CloseButton.Kind := bkCancel;
  CloseButton.Caption := 'Schliessen';
  CloseButton.OnClick := @CloseClick;
end;

destructor TLogForm.Destroy;
begin
  CloseButton.Free;
  RichMemo.Free;
  inherited Destroy;
end;

procedure TLogForm.Add(s: string; col: TColor);
begin
//  RichMemo.AddColorStr(s, col);
  RichMemo.Lines.Add(s, col);
  //  RichMemo.Lines.Add(s);
    Show;
end;

procedure TLogForm.FormResize(Sender: TObject);
begin
  RichMemo.Height := ClientHeight - 40;

  CloseButton.Left := ClientWidth - CloseButton.Width - 7;
  CloseButton.Top := ClientHeight - CloseButton.Height - 7;

  SaveButton.Left := CloseButton.Left - SaveButton.Width - 7;
  SaveButton.Top := CloseButton.Top;

  ClipbrdButton.Left := SaveButton.Left - CloseButton.Width - 7;
  ClipbrdButton.Top := CloseButton.Top;
end;

procedure TLogForm.SaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then begin
//    RichMemo.Lines.SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TLogForm.CloseClick(Sender: TObject);
begin
  Close;
end;

procedure TLogForm.ClipbrdClick(Sender: TObject);
begin
//  Clipboard.AsText := RichMemo.Lines.Text;
end;

initialization
  //LogForm := TLogForm.CreateNew(nil);
  LogForm := TLogForm.Create(nil);

finalization
  FreeAndNil(LogForm);
end.
