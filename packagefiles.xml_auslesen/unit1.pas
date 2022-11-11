unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Laz2_XMLCfg;

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
var
  XMLCFG: TXMLConfig;
  c, i: integer;
  s, ver: string;
begin
  Memo1.Clear;
  XMLCFG := TXMLConfig.Create(nil);
  XMLCFG.Filename := '/home/tux/fpcupdeluxe_stable/config_lazarus/packagefiles.xml';
  c := XMLCFG.GetValue('GlobalPkgLinks/Count', 10);

  Memo1.Lines.Add('PackagesCount: ' + c.ToString);
  Memo1.Lines.Add('');
  for i := 1 to c do begin
    s := 'GlobalPkgLinks/Item' + i.ToString + '/Name/Value';
    Memo1.Lines.Add('Name: ' + XMLCFG.GetValue(s, 'xxx'));

    s := 'GlobalPkgLinks/Item' + i.ToString + '/Version/Major';
    ver := 'Version: ' + XMLCFG.GetValue(s, '0') + '.';
    s := 'GlobalPkgLinks/Item' + i.ToString + '/Version/Minor';
    ver += XMLCFG.GetValue(s, '0') + '.';
    s := 'GlobalPkgLinks/Item' + i.ToString + '/Version/Release';
    ver += XMLCFG.GetValue(s, '0');
    Memo1.Lines.Add(ver);
    s := 'GlobalPkgLinks/Item' + i.ToString + '/LastUsed/Value';
    Memo1.Lines.Add('LastUsed: ' + XMLCFG.GetValue(s, ''));
    Memo1.Lines.Add('');
  end;
  XMLCFG.Free;
end;

end.
