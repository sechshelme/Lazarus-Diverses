unit Save_Combox_Box;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, StdCtrls, Controls, Classes, Dialogs, ComCtrls, Forms,
  XMLConf;

procedure ComboBox_Insert_Text(cb: TComboBox);

procedure LoadStrings_from_XML(Key: string; sl: TStrings; Default_Text: TStringArray = nil);
procedure SaveStrings_to_XML(Key: string; sl: TStrings);

procedure LoadComboBox_from_XML(cb: TComboBox; Default_Text: TStringArray = nil);
procedure SaveComboBox_to_XML(cb: TComboBox);


implementation

const
  Config_File = 'config.xml';
  maxComboBoxCount = 20;

function getParents(c: TWinControl): string;
var
  p: TWinControl;
begin
  Result := '';
  p := c;
  repeat
    Result := p.Name + '/' + Result;
    p := p.Parent;
  until p = nil;
end;

procedure ComboBox_Insert_Text(cb: TComboBox);
var
  i: integer;
  s: string;
begin
  s := cb.Text;
  i := cb.Items.IndexOf(s);
  if i >= 0 then begin
    cb.Items.Delete(i);
  end;

  cb.Items.Insert(0, s);

  if cb.Items.Count > maxComboBoxCount then begin
    cb.Items.Delete(cb.Items.Count - 1);
  end;

  cb.Text := s;
end;

procedure LoadStrings_from_XML(Key: string; sl: TStrings; Default_Text: TStringArray);
var
  Cfg: TXMLConfig;
  ct, i: integer;
  s: string;
begin
  Cfg := TXMLConfig.Create(nil);
  Cfg.Filename := Config_File;
  ct := Cfg.GetValue(Key + 'Count', 0);
  sl.Clear;

  for i := 0 to ct - 1 do begin
    s := Cfg.GetValue(Key + 'Item' + i.ToString + '/value', '');
    sl.Add(s);
  end;

  for i := 0 to Length(Default_Text) - 1 do begin
    if sl.Count < maxComboBoxCount then begin
      if sl.IndexOf(Default_Text[i]) < 0 then begin
        sl.Add(Default_Text[i]);
      end;
    end;
  end;

  Cfg.Free;
end;

procedure SaveStrings_to_XML(Key: string; sl: TStrings);
var
  Cfg: TXMLConfig;
  i: integer;
begin
  Cfg := TXMLConfig.Create(nil);
  Cfg.Filename := Config_File;
  Cfg.SetValue(Key + 'Count', sl.Count);
  for i := 0 to sl.Count - 1 do begin
    Cfg.SetValue(Key + 'Item' + i.ToString + '/value', sl[i]);
  end;
  Cfg.Free;
end;

procedure LoadComboBox_from_XML(cb: TComboBox; Default_Text: TStringArray);
var
  Key: string;
begin
  Key := getParents(cb);
  LoadStrings_from_XML(Key, cb.Items, Default_Text);
  if cb.Items.Count > 0 then begin
    cb.Text := cb.Items[0];
  end else begin
    cb.Text := '';
  end;
end;

procedure SaveComboBox_to_XML(cb: TComboBox);
var
  Key: string;
begin
  Key := getParents(cb);
  SaveStrings_to_XML(Key, cb.Items);
end;



end.

