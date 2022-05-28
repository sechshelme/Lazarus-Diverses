unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    FindFirst1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
var
  SLText, SL: TStringList;
  i: integer;

begin
  SL := FindAllFiles('../../', '*.lpi', True);



  SLText := TStringList.Create;
  for i := 0 to SL.Count - 1 do begin
    SLText.LoadFromFile(SL.Strings[i]);
    SLText.Text := StringReplace(SLText.Text,
      //    '<UnitOutputDirectory Value="$(PrimaryConfigPath)\lib"/>',
      '<UnitOutputDirectory Value="lib/$(TargetCPU)-$(TargetOS)"/>',
      '<UnitOutputDirectory Value="' + GetTempDir + '"/>',
      [rfReplaceAll, rfIgnoreCase]);


    SLText.Text := StringReplace(SLText.Text,
      '<Filename Value="' + GetTempDir + 'project1"/>',
      '<Filename Value="project1"/>',
      [rfReplaceAll, rfIgnoreCase]);
    Memo1.Lines.Add(SL.Strings[i]);
    SLText.SaveToFile(SL.Strings[i]);
  end;

  SL.Free;
  SLText.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := GetTempDir;
end;

end.
