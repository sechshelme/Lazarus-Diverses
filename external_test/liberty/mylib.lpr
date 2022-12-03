library mylib;

{$mode objfpc}{$H+}

  procedure Ausgabe(s: string); cdecl;
  begin
    WriteLn(s);
  end;

exports Ausgabe;

end.

