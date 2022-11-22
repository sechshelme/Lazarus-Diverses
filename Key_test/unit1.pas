unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLType;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  WriteLn('down ', Key);

end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  WriteLn('press ', byte(Key));

end;

procedure TForm1.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  WriteLn('utf8');
  WriteLn(UTF8Key);
end;

end.
