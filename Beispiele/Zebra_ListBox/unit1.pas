unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Types, LCLType;

type

  { TForm1 }

  TForm1 = class(TForm)
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
    ListBox1.Style := lbOwnerDrawVariable;
end;

procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TListBox).Canvas do begin
    if not (odSelected in State) then begin
      if Index mod 2 = 1 then begin
        Brush.Color := $00FFFFFF;
      end else begin
        Brush.Color := $00DDFFDD;
      end;
    end;

    FillRect(ARect);
    TextOut(ARect.Left, ARect.Top, (Control as TListBox).Items[Index]);
    if odFocused in State then begin
      Brush.Color := Color;
      DrawFocusRect(ARect);
    end;
  end;
end;

end.
