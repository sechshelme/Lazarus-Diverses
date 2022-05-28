program Hiscore_Liste_Loeschen;
uses
  Dos,crt;
const
  Datei = 'Liste.hic';
var
  i    : Byte;
  f    : text;
  ch   : char;
begin
  Writeln('Dieses Program L”scht die Hiscore - Liste von Pacman');
  Writeln;
  Writeln('Wollen Sie die Liste l”schen ? (J/N)');
  ch := Upcase(ReadKey);
  if ch = 'J' then begin
    Assign(f,Datei);
    Rewrite(f);
    for i := 20 downto 1 do begin
      Writeln(f,'  < Kein Eintrag >');
      Writeln(f,i*100000);
    end;
    close(f);
  end;
end.