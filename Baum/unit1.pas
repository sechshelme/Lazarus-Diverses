unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, oglVector;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormPaint(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormPaint(Sender: TObject);

  procedure Baum(ofs, vec: TVector2f; schach: integer);
  const
    r = 2.0;
    splitt = 3;
  var
    i: integer;
  begin
    Canvas.Line(round(ofs[0]), round(ofs[1]), round(ofs[0] + vec[0]), round(ofs[1] + vec[1]));
    ofs += vec;
    vec.Rotate(-r / 2 + Random * r);
    vec.Scale(0.5);

    if schach > 0 then begin
      for i := 0 to splitt do begin
        Baum(ofs, vec, schach - 1);
      end;
    end;

  end;

begin
  Baum(vec2(ClientWidth / 2, ClientHeight + 100), vec2(0, -ClientHeight div 2), 5);
  Baum(vec2(ClientWidth / 2, ClientHeight + 100), vec2(0, -ClientHeight div 2), 5);

end;

end.
