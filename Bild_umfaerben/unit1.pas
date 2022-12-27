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

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  bit: TPicture;
  x, y: integer;
begin
  bit := TPicture.Create;
  bit.LoadFromFile('foo.png');
  with bit.Bitmap do begin
    for x := 0 to Width do begin
      for y := 0 to Height do begin
        if Canvas.Pixels[x, y] = $FFFFFF then begin
          Canvas.Pixels[x, y] := $FFFF00;
        end;
      end;
    end;
  end;
  bit.SaveToFile('foo2.png');
  bit.Free;
end;

end.
