unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, FileUtil;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
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
const
  path = '/home/tux/Schreibtisch/View';
var
  fl: TStringList;
  i: integer;
begin
  Memo1.Clear;
  fl := FindAllFiles(path, '*.rpm', False);
  for i := 0 to fl.Count - 1 do begin
    Memo1.Lines.Add('fakeroot alien ' + fl[i]);
  end;
  Memo1.Lines.SaveToFile(path + '/convert.sh');
  fl.Free;
end;

end.
