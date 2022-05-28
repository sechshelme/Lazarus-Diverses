program TextToPascal;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils;

  procedure Erzeugen(Datei: string);
  var
    StrInput, StrOutput: TStringList;
    i: integer;
    s: string;
    sOutput: string = #13#10 + '    ';
  begin

    StrInput := TStringList.Create;
    StrOutput := TStringList.Create;
    StrInput.LoadFromFile(Datei);

    //    StringReplace(Datei, '.','_',[rfReplaceAll]);

    Datei[Pos('.', Datei)] := '_';

    //    Delete(Datei, Pos('.', Datei), 255);

    for i := 1 to Length(StrInput.Text) do begin
      if (i - 2) mod 18 = 17 then begin
        sOutPut := sOutput + '+' + #13#10 + '    ';
      end;
      s := IntToStr(byte(StrInput.Text[i]));
      sOutput := sOutput + '#' + StringOfChar('0', 3 - Length(s)) + s;
    end;

    StrOutput.Add('const');
    StrOutput.Add('  Str_' + Datei + ' = ' + sOutput + ';');

    Writeln(StrOutput.Text);
    StrOutput.SaveToFile(Datei + '.inc');
    StrInput.Free;
    StrOutput.Free;
  end;

var
  i: integer;
begin
  if ParamCount = 0 then begin
    Writeln('Erzeugt aus einer Text-Datei einen Pascal-String.' + #13#10);
    Writeln('TEXTTOPASCAL TextDatei, [TextDatei2], ...');
  end else begin
    for i := 1 to ParamCount do begin
      Erzeugen(ParamStr(i));
    end;
  end;
end.
